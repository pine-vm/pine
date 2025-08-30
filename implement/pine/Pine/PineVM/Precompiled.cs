using ElmTime.ElmInteractive;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.Internal;
using Pine.Core.Json;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using System;
using System.Buffers.Binary;
using System.Collections.Concurrent;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

public class Precompiled
{
    /*
     * 
     * Using the general environment class model in the first stage of dispatch/linking means that we can use that
     * information to optimize the dispatch at compile time.
     * */

    public abstract record PrecompiledResult
    {
        public sealed record FinalValue(
            PineValue Value,
            long StackFrameCount)
            : PrecompiledResult;

        public sealed record ContinueParseAndEval(
            PineValue EnvironmentValue,
            PineValue ExpressionValue)
            : PrecompiledResult;

        public sealed record StepwiseSpecialization(
            PineVM.ApplyStepwise Stepwise)
            : PrecompiledResult;
    }

    record PrecompiledEntry(
        PineValueClass EnvConstraint,
        Func<PineValue, PineVMParseCache, Func<PrecompiledResult>?> PrecompiledDelegate)
    {
        public PrecompiledEntry(
            PineValueClass EnvConstraint,
            Func<PineValue, PineVMParseCache, PrecompiledResult?> PrecompiledDelegate)
            : this(
                EnvConstraint,
                (env, parseCache) =>
                {
                    var result = PrecompiledDelegate(env, parseCache);

                    if (result is null)
                    {
                        return null;
                    }

                    return () => result;
                })
        {
        }

        public static PrecompiledEntry FinalValueForAnyEnvironment(
            PineValueClass EnvConstraint,
            Func<PineValue, PineVMParseCache, PineValue> delegateForAnyEnv) =>
            new(
                EnvConstraint,
                (env, parseCache) =>
                () => new PrecompiledResult.FinalValue(delegateForAnyEnv(env, parseCache), StackFrameCount: 0));
    }

    public static ImmutableHashSet<Expression> PrecompiledExpressions =>
        PrecompiledDict?.Keys.ToImmutableHashSet() ?? [];

    public static bool HasPrecompiledForExpression(Expression expression) =>
        PrecompiledDict?.ContainsKey(expression) ?? false;

    public static Func<PrecompiledResult>? SelectPrecompiled(
        Expression expression,
        PineValue environment,
        PineVMParseCache parseCache)
    {
        if (PrecompiledDict?.TryGetValue(expression, out var envItems) ?? false)
        {
            /*
            {
                // Ensure that the key is the same instance as the expression.
                // This should always be the case when we include the precompiled expression dictionary into the source of reused instances.
                var actualKey = PrecompiledDict.Keys.First(key => key == expression);

                if (!ReferenceEquals(actualKey, expression))
                {
                    throw new InvalidOperationException(
                        "PrecompiledDict key is not the same instance as the expression: " + actualKey);
                }
            }
            */

            for (var i = 0; i < envItems.Count; ++i)
            {
                var envItem = envItems[i];

                if (envItem.EnvConstraint.SatisfiedByValue(environment))
                {
                    return envItem.PrecompiledDelegate.Invoke(environment, parseCache);
                }
            }
        }

        return null;
    }

    private static IEnumerable<KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>> BuildPrecompiled()
    {
        var adaptivePartialApplicationExpression =
            popularExpressionDictionary["adaptivePartialApplication"];

        var adaptivePartialApplicationExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(adaptivePartialApplicationExpression);

        var isPineListExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["isPineList"]);

        var isPineBlobExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["isPineBlob"]);

        var compareStringsExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["compareStrings"]);

        var compareExpression =
            popularExpressionDictionary["compare"];

        var compareExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(compareExpression);

        var compareListsExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["compareLists"]);

        var eqExpression =
            popularExpressionDictionary["eq"];

        var eqExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(eqExpression);

        var dictToListExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["dictToList"]);

        var dictKeysExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["dictKeys"]);

        var listsEqualRecursiveExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["listsEqualRecursive"]);


        var listMemberExpression = popularExpressionDictionary["List.member"];

        var listMemberExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listMemberExpression);

        var basicsEqExposedValue = popularValueDictionary["Basics.eq.exposed"];

        var listMemberExposedValue = popularValueDictionary["List.member.exposed"];

        var listMapExposedValue = popularValueDictionary["List.map.exposed"];

        var stringSliceExpression = popularExpressionDictionary["String.slice"];

        var stringSliceExposedValue = popularValueDictionary["String.slice.exposed"];

        var dictGetExpression = popularExpressionDictionary["dictGet"];

        var dictGetExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictGetExpression);

        var dictGetAfterCompareExpression = popularExpressionDictionary["dictGetAfterCompare"];

        var dictGetAfterCompareExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictGetAfterCompareExpression);


        var compareExposedValue = popularValueDictionary["Basics.compare.exposed"];

        var idivExposedValue = popularValueDictionary["Basics.idiv.exposed"];


        var assocListGetExpression = popularExpressionDictionary["assocListGet"];

        var assocListGetExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(assocListGetExpression);

        var commonAssocListGetWithIndexHelperExpression =
            popularExpressionDictionary["Common.assocListGetWithIndexHelper"];

        var commonAssocListGetWithIndexHelperExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(commonAssocListGetWithIndexHelperExpression);

        var commonListUniqueHelpExpression =
            popularExpressionDictionary["Common.listUniqueHelp"];

        var commonListUniqueHelpExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(commonListUniqueHelpExpression);

        var dictSizeHelpExpression = popularExpressionDictionary["dictSizeHelp"];

        var dictSizeHelpExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictSizeHelpExpression);


        var elmCompiledRecordAccessExpression = popularExpressionDictionary["elmCompiledRecordAccess"];

        var elmCompiledRecordAccessExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(elmCompiledRecordAccessExpression);

        var countPineListValueContentExpression =
            popularExpressionDictionary["countPineListValueContent"];

        var countPineListValueContentExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(countPineListValueContentExpression);

        var countPineValueContentExpression =
            popularExpressionDictionary["countPineValueContent"];

        var countPineValueContentExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(countPineValueContentExpression);

        var listMapHelpExpression =
            popularExpressionDictionary["List.mapHelp"];

        var listMapHelpExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listMapHelpExpression);

        var listConcatMapExpression =
            popularExpressionDictionary["List.concatMap"];

        var listConcatMapExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listConcatMapExpression);

        var listFoldlExpression =
            popularExpressionDictionary["List.foldl"];

        var listFoldlExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listFoldlExpression);

        var listFilterHelpExpression =
            popularExpressionDictionary["List.filterHelp"];

        var listFilterHelpExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listFilterHelpExpression);

        var listAllExpression =
            popularExpressionDictionary["List.all"];

        var listAllExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listAllExpression);

        var listAnyExpression =
            popularExpressionDictionary["List.any"];

        var listAnyExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listAnyExpression);

        var elmKernelParser_countOffsetsInStringExpression =
            popularExpressionDictionary["Elm.Kernel.Parser.countOffsetsInString"];

        var elmKernelParser_countOffsetsInStringExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(elmKernelParser_countOffsetsInStringExpression);

        var elmKernelParser_chompWhileHelpExpression =
            popularExpressionDictionary["Elm.Kernel.Parser.chompWhileHelp"];

        var elmKernelParser_chompWhileHelpExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(elmKernelParser_chompWhileHelpExpression);

        var elmKernelParserFindSubStringExpression =
            popularExpressionDictionary["Elm.Kernel.Parser.findSubString"];

        var elmKernelParserFindSubStringExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(elmKernelParserFindSubStringExpression);

        var elmKernelParserIndexOfExpression =
            popularExpressionDictionary["Elm.Kernel.Parser.indexOf"];

        var elmKernelParserIndexOfExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(elmKernelParserIndexOfExpression);


        {
            var compareExpressionEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        isPineListExpressionValue,
                        compareStringsExpressionValue,
                        compareExpressionValue,
                        compareListsExpressionValue
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    compareExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(compareExpressionEnvClass, BasicsCompare)]);
        }

        {
            var eqExpressionEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        isPineBlobExpressionValue,
                        dictKeysExpressionValue,
                        dictToListExpressionValue,
                        eqExpressionValue,
                        listsEqualRecursiveExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    eqExpression,
                    [new PrecompiledEntry(eqExpressionEnvClass, BasicsEq)]);
        }


        {
            var listMemberExpressionEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        basicsEqExposedValue,
                        listMemberExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listMemberExpression,
                    [new PrecompiledEntry(listMemberExpressionEnvClass, ListMember)]);
        }

        {
            var listMapHelpEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        listMapHelpExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listMapHelpExpression,
                    [new PrecompiledEntry(
                        listMapHelpEnvClass,
                        ListMapHelp)]);
        }

        {
            var concatMapEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        adaptivePartialApplicationExpressionValue,
                        listConcatMapExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listConcatMapExpression,
                    [new PrecompiledEntry(
                        concatMapEnvClass,
                        ListConcatMap)]);
        }

        {
            var listFoldlEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0,0],
                    listFoldlExpressionValue),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listFoldlExpression,
                    [new PrecompiledEntry(
                        listFoldlEnvClass,
                        ListFoldl)]);
        }

        {
            var listFilterEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        adaptivePartialApplicationExpressionValue,
                        listFilterHelpExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listFilterHelpExpression,
                    [new PrecompiledEntry(
                        listFilterEnvClass,
                        ListFilterHelp)]);
        }

        {
            var listFilterMapHelpExpression =
                popularExpressionDictionary["List.filterMapHelp"];

            var listFilterMapHelpExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(listFilterMapHelpExpression);

            var listFilterMapEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        adaptivePartialApplicationExpressionValue,
                        listFilterMapHelpExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listFilterMapHelpExpression,
                    [new PrecompiledEntry(
                        listFilterMapEnvClass,
                        ListFilterMapHelp)]);
        }

        {
            var listAllEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        adaptivePartialApplicationExpressionValue,
                        listAllExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listAllExpression,
                    [new PrecompiledEntry(
                        listAllEnvClass,
                        ListAll)]);
        }

        {
            var listAnyEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        adaptivePartialApplicationExpressionValue,
                        listAnyExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listAnyExpression,
                    [new PrecompiledEntry(
                        listAnyEnvClass,
                        ListAny)]);
        }

        {
            var listIntersperseHelpExpression =
                popularExpressionDictionary["List.intersperseHelp"];

            var listIntersperseHelpExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(listIntersperseHelpExpression);

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    listIntersperseHelpExpressionValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listIntersperseHelpExpression,
                    [new PrecompiledEntry(
                        envClass,
                        ListIntersperseHelp)]);
        }

        {
            var stringSliceEnvClass =
                PineValueClass.Create(
                    [
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    stringSliceExpression,
                    [new PrecompiledEntry(
                        stringSliceEnvClass,
                        StringSlice)]);
        }

        {
            var stringLinesHelperExpression = popularExpressionDictionary["String.linesHelper"];

            var stringLinesHelperExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(stringLinesHelperExpression);

            var stringLinesHelperEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    stringLinesHelperExpressionValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    stringLinesHelperExpression,
                    [new PrecompiledEntry(
                        stringLinesHelperEnvClass,
                        StringLinesHelper)]);
        }

        {
            var stringToListRecursiveExpression = popularExpressionDictionary["String.toListRecursive"];

            var stringToListRecursiveExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(stringToListRecursiveExpression);

            var stringToListRecursiveEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    stringToListRecursiveExpressionValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    stringToListRecursiveExpression,
                    [new PrecompiledEntry(
                        stringToListRecursiveEnvClass,
                        StringToListRecursive)]);
        }

        {
            var splitHelperOnBlobExpression = popularExpressionDictionary["String.splitHelperOnBlob"];

            var splitHelperOnBlobExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(splitHelperOnBlobExpression);

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    splitHelperOnBlobExpressionValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    splitHelperOnBlobExpression,
                    [new PrecompiledEntry(
                        envClass,
                        String_splitHelperOnBlob)]);
        }

        {
            var skipWhitespaceExpression = popularExpressionDictionary["Elm.Kernel.Json.Decode.skipWhitespace"];

            var skipWhitespaceExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(skipWhitespaceExpression);

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    skipWhitespaceExpressionValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    skipWhitespaceExpression,
                    [new PrecompiledEntry(
                        envClass,
                        Elm_Kernel_Json_Decode_skipWhitespace)]);
        }

        {
            var assocListGetExpressionEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        basicsEqExposedValue,
                        assocListGetExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    assocListGetExpression,
                    [new PrecompiledEntry(assocListGetExpressionEnvClass, CommonAssocListGet)]);
        }

        {
            var commonAssocListGetWithIndexHelperEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        basicsEqExposedValue,
                        commonAssocListGetWithIndexHelperExpressionValue,
                        ])),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    commonAssocListGetWithIndexHelperExpression,
                    [new PrecompiledEntry(commonAssocListGetWithIndexHelperEnvClass, CommonAssocListGetWithIndexHelper)]);
        }

        {
            var listUniqueHelpExpressionEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        listMemberExposedValue,
                        commonListUniqueHelpExpressionValue
                        ])),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    commonListUniqueHelpExpression,
                    [new PrecompiledEntry(listUniqueHelpExpressionEnvClass, CommonListUniqueHelp)]);
        }


        {
            var dictGetExpressionEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        compareExposedValue,
                        dictGetExpressionValue,
                        dictGetAfterCompareExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    dictGetExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(dictGetExpressionEnvClass, DictGet)]);
        }


        {
            var dictSizeHelpEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        adaptivePartialApplicationExpressionValue,
                        dictSizeHelpExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    dictSizeHelpExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(dictSizeHelpEnvClass, DictSizeHelp)]);
        }


        {
            var elmCompiledRecordAccessEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    elmCompiledRecordAccessExpressionValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmCompiledRecordAccessExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(elmCompiledRecordAccessEnvClass, ElmCompiledRecordAccess)]);
        }


        {
            var adaptivePartialApplicationEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    adaptivePartialApplicationExpressionValue),

                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [1,0],
                    StringEncoding.ValueFromString("Function")),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    adaptivePartialApplicationExpression,
                    [new PrecompiledEntry(adaptivePartialApplicationEnvClass, ElmCompiledAdaptivePartialApplication)]);
        }

        {
            var parseJsonStringSimpleCharsExpr =
                popularExpressionDictionary["Elm.Kernel.Json.Decode.parseJsonStringSimpleChars"];

            var parseJsonStringSimpleCharsExprValue =
                ExpressionEncoding.EncodeExpressionAsValue(parseJsonStringSimpleCharsExpr);

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 0],
                        parseJsonStringSimpleCharsExprValue),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    parseJsonStringSimpleCharsExpr,
                    [new PrecompiledEntry(
                        envClass,
                        ElmKernelJsonDecode_parseJsonStringSimpleChars)]);
        }

        {
            var countPineValueContentEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        countPineValueContentExpressionValue,
                        countPineListValueContentExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    countPineValueContentExpression,
                    [new PrecompiledEntry(countPineValueContentEnvClass, CountPineValueContent)]);
        }

        {
            var computeValueFromString_2025Expr =
                popularExpressionDictionary["Pine.computeValueFromString_2025.body"];

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0],
                        popularValueDictionary["Pine.computeValueFromString_2025.aggregate-env"]),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    computeValueFromString_2025Expr,
                    [new PrecompiledEntry(
                        envClass,
                        PineComputeValueFromString_2025)]);
        }


        {
            var countOffsetsInStringEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    elmKernelParser_countOffsetsInStringExpressionValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmKernelParser_countOffsetsInStringExpression,
                    [new PrecompiledEntry(
                        countOffsetsInStringEnvClass,
                        ElmKernelParser_countOffsetsInString)]);
        }

        {
            var findSubStringEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    elmKernelParser_countOffsetsInStringExpressionValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 1],
                    elmKernelParserIndexOfExpressionValue),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmKernelParserFindSubStringExpression,
                    [new PrecompiledEntry(
                        findSubStringEnvClass,
                        ElmKernelParser_findSubstring)]);
        }

        {
            /*
             * chompWhileHelp : (Char -> Bool) -> ( Int, Int, Int ) -> List Char -> ( Int, Int, Int )
             * chompWhileHelp isGood ( offset, row, col ) srcChars =
             * */

            var chompWhileHelpEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 0],
                        adaptivePartialApplicationExpressionValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 1],
                        elmKernelParser_chompWhileHelpExpressionValue),
                ]);

            PineValueClass chompWhileHelpEnvClassFromPredicate(PineValue predicateValue)
            {
                return
                    PineValueClass.Create(
                        [..chompWhileHelpEnvClass.ParsedItems
                        ,new KeyValuePair<IReadOnlyList<int>, PineValue>(
                            [1, 0],
                            predicateValue)]);
            }

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmKernelParser_chompWhileHelpExpression,
                    [new PrecompiledEntry(
                        chompWhileHelpEnvClassFromPredicate(
                            popularValueDictionary["predicate_first_arg_equals_ASCII_char_space_32"]),
                        ElmKernelParser_chompWhileHelp_single_char_space_32)]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmKernelParser_chompWhileHelpExpression,
                    [new PrecompiledEntry(
                        chompWhileHelpEnvClassFromPredicate(
                            popularValueDictionary["predicate_first_arg_equals_ASCII_char_minus_45"]),
                        ElmKernelParser_chompWhileHelp_single_char_minus_45)]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmKernelParser_chompWhileHelpExpression,
                    [new PrecompiledEntry(
                        chompWhileHelpEnvClassFromPredicate(
                            popularValueDictionary["predicate_first_arg_is_not_ASCII_carriage_return_or_newline"]),
                        ElmKernelParser_chompWhileHelp_not_carriage_return_or_newline)]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmKernelParser_chompWhileHelpExpression,
                    [new PrecompiledEntry(
                        chompWhileHelpEnvClassFromPredicate(
                            popularValueDictionary["predicate_nestableComment_char_is_not_elm_multiline_comment_open_or_close"]),
                        ElmKernelParser_chompWhileHelp_not_elm_multiline_comment_open_or_close)]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmKernelParser_chompWhileHelpExpression,
                    [new PrecompiledEntry(
                        chompWhileHelpEnvClassFromPredicate(
                            popularValueDictionary["predicate_parser_char_is_alpha_num_or_underscore"]),
                        ElmKernelParser_chompWhileHelp_is_alpha_num_or_underscore)]);

            /*
            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmKernelParser_chompWhileHelpExpression,
                    [new PrecompiledEntry(
                        chompWhileHelpEnvClass,
                        ElmKernelParser_chompWhileHelp)]);
            */
        }

        {
            var skipWhileWhitespaceHelpExpression =
                popularExpressionDictionary["ParserFast.skipWhileWhitespaceHelp"];

            var skipWhileWhitespaceHelpExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(skipWhileWhitespaceHelpExpression);

            var skipWhileWhitespaceHelpEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 0],
                        adaptivePartialApplicationExpressionValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 1],
                        skipWhileWhitespaceHelpExpressionValue),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    skipWhileWhitespaceHelpExpression,
                    [new PrecompiledEntry(
                        skipWhileWhitespaceHelpEnvClass,
                        ParserFast_skipWhileWhitespaceHelp)]);
        }

        {
            var skipWhileWithoutLinebreakHelpExpression =
                popularExpressionDictionary["ParserFast.skipWhileWithoutLinebreakHelp"];

            var skipWhileWithoutLinebreakHelpExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(skipWhileWithoutLinebreakHelpExpression);

            var skipWhileWithoutLinebreakHelpEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0,0],
                        adaptivePartialApplicationExpressionValue),

                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0,1],
                        skipWhileWithoutLinebreakHelpExpressionValue),
                    ]);

            PineValueClass envClassFromPredicate(PineValue predicateValue)
            {
                return
                    PineValueClass.Create(
                        [..skipWhileWithoutLinebreakHelpEnvClass.ParsedItems
                        ,new KeyValuePair<IReadOnlyList<int>, PineValue>(
                            [1, 0],
                            predicateValue)]);
            }

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    skipWhileWithoutLinebreakHelpExpression,
                    [new PrecompiledEntry(
                        envClassFromPredicate(
                            popularValueDictionary["ParserFast.isAlphaNumOrUnderscore.exposed"]),
                        ParserFast_skipWhileWithoutLinebreakHelp_isAlphaNumOrUnderscore)]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    skipWhileWithoutLinebreakHelpExpression,
                    [new PrecompiledEntry(
                        envClassFromPredicate(
                            popularValueDictionary["predicate_first_arg_is_not_ASCII_quote_or_backslash"]),
                        ParserFast_skipWhileWithoutLinebreakHelp_is_not_ASCII_quote_or_backslash)]);

            /*
            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    skipWhileWithoutLinebreakHelpExpression,
                    [new PrecompiledEntry(
                        skipWhileWithoutLinebreakHelpEnvClass,
                        ParserFast_skipWhileWithoutLinebreakHelp)]);
            */
        }

        {
            var skipWhileHelpExpression =
                popularExpressionDictionary["ParserFast.skipWhileHelp"];

            var skipWhileHelpExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(skipWhileHelpExpression);

            var skipWhileHelpEnvClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0,0],
                        adaptivePartialApplicationExpressionValue),

                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0,1],
                        skipWhileHelpExpressionValue),
                    ]);

            PineValueClass envClassFromPredicate(PineValue predicateValue)
            {
                return
                    PineValueClass.Create(
                        [..skipWhileHelpEnvClass.ParsedItems
                        ,new KeyValuePair<IReadOnlyList<int>, PineValue>(
                            [1, 0],
                            predicateValue)]);
            }

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    skipWhileHelpExpression,
                    [new PrecompiledEntry(
                        envClassFromPredicate(
                            popularValueDictionary["predicate_nestableComment_char_is_not_elm_multiline_comment_open_or_close"]),
                        ParserFast_skipWhileHelp_not_elm_multiline_comment_open_or_close)]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    skipWhileHelpExpression,
                    [new PrecompiledEntry(
                        envClassFromPredicate(
                            popularValueDictionary["predicate_first_arg_is_not_ASCII_quote_or_backslash"]),
                        ParserFast_skipWhileHelp_not_ASCII_quote_or_backslash)]);

            /*
            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    skipWhileHelpExpression,
                    [new PrecompiledEntry(
                        skipWhileHelpEnvClass,
                        ParserFast_skipWhileHelp)]);
            */
        }

        {
            var prependToExpression =
                popularExpressionDictionary["Rope.prependTo"];

            var prependToExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(prependToExpression);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    prependToExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(
                        PineValueClass.Create([]),
                        RopePrependTo)]);
        }

        var charToCodeExpression =
            popularExpressionDictionary["Char.toCode"];

        var charToCodeExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(charToCodeExpression);

        {
            var isLowerExpression =
                popularExpressionDictionary["Char.isLower"];

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        charToCodeExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    isLowerExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(
                        envClass,
                        CharIsLower)]);
        }

        {
            var isUpperExpression =
                popularExpressionDictionary["Char.isUpper"];

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        charToCodeExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    isUpperExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(
                        envClass,
                        CharIsUpper)]);
        }

        {
            var encodeCharAsBlobExpression =
                popularExpressionDictionary["Bytes.Encode.encodeCharAsBlob"];

            var encodeCharAsBlobExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(encodeCharAsBlobExpression);

            var encodeCharsAsBlobHelpExpression =
                popularExpressionDictionary["Bytes.Encode.encodeCharsAsBlobHelp"];

            var encodeCharsAsBlobHelpExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(encodeCharsAsBlobHelpExpression);

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    encodeCharAsBlobExpressionValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 1],
                    idivExposedValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 2],
                    encodeCharsAsBlobHelpExpressionValue),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    encodeCharsAsBlobHelpExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(
                        envClass,
                        BytesEncode_encodeCharsAsBlobHelp)]);
        }

        {
            var decodeBlobAsCharsRecExpression =
                popularExpressionDictionary["Bytes.Decode.decodeBlobAsCharsRec"];

            var decodeBlobAsCharsRecExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(decodeBlobAsCharsRecExpression);

            var decodeUtf8CharExpression =
                popularExpressionDictionary["Bytes.Decode.decodeUtf8Char"];

            var decodeUtf8CharExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(decodeUtf8CharExpression);

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    decodeUtf8CharExpressionValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 1],
                    decodeBlobAsCharsRecExpressionValue),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    decodeBlobAsCharsRecExpression,
                    [new PrecompiledEntry(
                        envClass,
                        BytesDecode_decodeBlobAsCharsRec)]);
        }

        {
            var bytesEncodeEncodeBlobExpression =
                popularExpressionDictionary["Bytes.Encode.encodeBlob"];

            var bytesEncodeEncodeBlobExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(bytesEncodeEncodeBlobExpression);

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 0],
                    listMapExposedValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0, 1],
                    bytesEncodeEncodeBlobExpressionValue),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    bytesEncodeEncodeBlobExpression,
                    [new PrecompiledEntry(
                        envClass,
                        BytesEncode_encodeBlob)]);
        }

        {
            var encodeToBytesBodyExpression =
                popularExpressionDictionary["danfishgold.Base64.Encode.toBytes.body"];

            var aggregateEnvFuncValue =
                popularValueDictionary["danfishgold.Base64.Encode.toBytes.aggregate-env-funcs"];

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    aggregateEnvFuncValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    encodeToBytesBodyExpression,
                    [new PrecompiledEntry(
                        envClass,
                        Danfishgold_Base64_Encode_toBytes)]);
        }

        {
            var decodeFromBytesBodyExpression =
                popularExpressionDictionary["danfishgold.Base64.Decode.fromBytes.body"];

            var aggregateEnvFuncValue =
                popularValueDictionary["danfishgold.Base64.Decode.fromBytes.aggregate-env-funcs"];

            var envClass =
                PineValueClass.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    aggregateEnvFuncValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    decodeFromBytesBodyExpression,
                    [new PrecompiledEntry(
                        envClass,
                        Danfishgold_Base64_Decode_fromBytes)]);
        }
    }

    static PineValue RopePrependTo(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        prependTo : Rope a -> Rope a -> Rope a
        prependTo right left =
            case left of
                Nothing ->
                    right

                Just leftLikelyFilled ->
                    case right of
                        Nothing ->
                            left

                        Just rightLikelyFilled ->
                            Just (Branch2 leftLikelyFilled rightLikelyFilled)

         * */

        var right =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var left =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var leftTagName =
            PineVM.ValueFromPathInValueOrEmptyList(left, [0]);

        if (leftTagName == Tag_Nothing_Name_Value)
        {
            return right;
        }

        var rightTagName =
            PineVM.ValueFromPathInValueOrEmptyList(right, [0]);

        if (rightTagName == Tag_Nothing_Name_Value)
        {
            return left;
        }

        var leftLikelyFilled =
            PineVM.ValueFromPathInValueOrEmptyList(left, [1, 0]);

        var rightLikelyFilled =
            PineVM.ValueFromPathInValueOrEmptyList(right, [1, 0]);

        return Tag_Just_Value(
            PineValue.List(
                [
                Tag_Branch2_Name_Value,
                PineValue.List([leftLikelyFilled, rightLikelyFilled])
                ]));
    }

    static PineValue CharIsLower(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        isLower : Char -> Bool
        isLower char =
            let
                code =
                    toCode char
            in
            Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ]

         * */

        var charValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        if (charValue is not PineValue.BlobValue charBlob || charBlob.Bytes.Length < 1)
        {
            return PineKernelValues.FalseValue;
        }

        for (var i = 0; i < charBlob.Bytes.Length - 1; ++i)
        {
            if (charBlob.Bytes.Span[i] is not 0)
            {
                return PineKernelValues.FalseValue;
            }
        }

        var charCodeLowByte = charBlob.Bytes.Span[^1];

        var isLower =
            0x61 <= charCodeLowByte &&
            charCodeLowByte <= 0x7A;

        return
            isLower
            ?
            PineKernelValues.TrueValue
            :
            PineKernelValues.FalseValue;
    }

    static PineValue CharIsUpper(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var charValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        if (charValue is not PineValue.BlobValue charBlob || charBlob.Bytes.Length < 1)
        {
            return PineKernelValues.FalseValue;
        }

        for (var i = 0; i < charBlob.Bytes.Length - 1; ++i)
        {
            if (charBlob.Bytes.Span[i] is not 0)
            {
                return PineKernelValues.FalseValue;
            }
        }

        var charCodeLowByte = charBlob.Bytes.Span[^1];

        var isUpper =
            0x41 <= charCodeLowByte &&
            charCodeLowByte <= 0x5A;

        return
            isUpper
            ?
            PineKernelValues.TrueValue
            :
            PineKernelValues.FalseValue;
    }

    static PineValue BytesEncode_encodeCharsAsBlobHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        encodeCharsAsBlobHelp : Int -> Int -> Int -> Int
        encodeCharsAsBlobHelp acc offset charsBytes =
            let
                nextChar =
                    Pine_kernel.take [ 4, Pine_kernel.skip [ offset, charsBytes ] ]
            in
            if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                acc

            else
                encodeCharsAsBlobHelp
                    (Pine_kernel.concat [ acc, encodeCharAsBlob nextChar ])
                    (Pine_kernel.int_add [ offset, 4 ])
                    charsBytes


        encodeCharAsBlob : Char -> Int
        encodeCharAsBlob char =
            let
                code =
                    Char.toCode char
            in
            if Pine_kernel.int_is_sorted_asc [ code, 0x7F ] then
                -- 1-byte encoding
                Pine_kernel.skip
                    [ 1
                    , code
                    ]

            else if Pine_kernel.int_is_sorted_asc [ code, 0x07FF ] then
                -- 2-byte encoding
                let
                    byte1 =
                        Pine_kernel.bit_or
                            [ 0xC0
                            , code // 64
                            ]

                    byte2 =
                        Pine_kernel.bit_or
                            [ maskSingleByteMSB
                            , Pine_kernel.bit_and [ 63, code ]
                            ]
                in
                Pine_kernel.concat
                    [ Pine_kernel.bit_and [ byte1, maskSingleByte ]
                    , Pine_kernel.bit_and [ byte2, maskSingleByte ]
                    ]

            else if Pine_kernel.int_is_sorted_asc [ code, 0xFFFF ] then
                -- 3-byte encoding
                let
                    byte1 =
                        Pine_kernel.bit_or
                            [ 0xE0
                            , code // 4096
                            ]

                    byte2 =
                        Pine_kernel.bit_or
                            [ maskSingleByteMSB
                            , Pine_kernel.bit_and [ 63, code // 64 ]
                            ]

                    byte3 =
                        Pine_kernel.bit_or
                            [ maskSingleByteMSB
                            , Pine_kernel.bit_and [ 63, code ]
                            ]
                in
                Pine_kernel.concat
                    [ Pine_kernel.bit_and [ byte1, maskSingleByte ]
                    , Pine_kernel.bit_and [ byte2, maskSingleByte ]
                    , Pine_kernel.bit_and [ byte3, maskSingleByte ]
                    ]

            else
                -- 4-byte encoding for code points >= 0x10000
                let
                    byte1 =
                        Pine_kernel.bit_or
                            [ Pine_kernel.bit_and [ 0xF0, maskSingleByte ]
                            , code // 262144
                            ]

                    byte2 =
                        Pine_kernel.bit_or
                            [ maskSingleByteMSB
                            , Pine_kernel.bit_and [ 63, code // 4096 ]
                            ]

                    byte3 =
                        Pine_kernel.bit_or
                            [ maskSingleByteMSB
                            , Pine_kernel.bit_and [ 63, code // 64 ]
                            ]

                    byte4 =
                        Pine_kernel.bit_or
                            [ maskSingleByteMSB
                            , Pine_kernel.bit_and [ 63, code ]
                            ]
                in
                Pine_kernel.concat
                    [ Pine_kernel.bit_and [ byte1, maskSingleByte ]
                    , Pine_kernel.bit_and [ byte2, maskSingleByte ]
                    , Pine_kernel.bit_and [ byte3, maskSingleByte ]
                    , Pine_kernel.bit_and [ byte4, maskSingleByte ]
                    ]

         * */

        var acc =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var chars =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(offsetValue).IsOkOrNullable() is not { } offset)
        {
            return acc;
        }

        if (chars == PineValue.EmptyList)
        {
            return acc;
        }

        if (chars is not PineValue.BlobValue charsBlob)
        {
            return acc;
        }

        if (charsBlob.Bytes.Length is 0)
        {
            return acc;
        }

        var accBlob =
            acc as PineValue.BlobValue ?? PineValue.EmptyBlob;

        var buffer = new byte[accBlob.Bytes.Length + charsBlob.Bytes.Length];

        accBlob.Bytes.CopyTo(buffer);

        var bytesWritten = accBlob.Bytes.Length;

        var offsetInt = (int)offset;

        while (true)
        {
            if (offsetInt + 4 > charsBlob.Bytes.Length)
            {
                break;
            }

            var charCode =
                BinaryPrimitives.ReadInt32BigEndian(charsBlob.Bytes.Span[offsetInt..]);

            if (charCode <= 0x7F)
            {
                buffer[bytesWritten++] = (byte)charCode;
            }
            else if (charCode <= 0x7FF)
            {
                buffer[bytesWritten++] = (byte)(0xC0 | (charCode >> 6));
                buffer[bytesWritten++] = (byte)(0x80 | (charCode & 0x3F));
            }
            else if (charCode <= 0xFFFF)
            {
                buffer[bytesWritten++] = (byte)(0xE0 | (charCode >> 12));
                buffer[bytesWritten++] = (byte)(0x80 | ((charCode >> 6) & 0x3F));
                buffer[bytesWritten++] = (byte)(0x80 | (charCode & 0x3F));
            }
            else
            {
                buffer[bytesWritten++] = (byte)(0xF0 | (charCode >> 18));
                buffer[bytesWritten++] = (byte)(0x80 | ((charCode >> 12) & 0x3F));
                buffer[bytesWritten++] = (byte)(0x80 | ((charCode >> 6) & 0x3F));
                buffer[bytesWritten++] = (byte)(0x80 | (charCode & 0x3F));
            }

            offsetInt += 4;
        }

        return PineValue.Blob(buffer.AsMemory(0, bytesWritten));
    }

    static PrecompiledResult.FinalValue? BytesDecode_decodeBlobAsCharsRec(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        decodeBlobAsCharsRec : Int -> Int -> List Char -> String
        decodeBlobAsCharsRec offset blob chars =
            if Pine_kernel.int_is_sorted_asc [ Pine_kernel.length blob, offset ] then
                String.fromList (List.reverse chars)

            else
                let
                    ( char, bytesConsumed ) =
                        decodeUtf8Char blob offset
                in
                decodeBlobAsCharsRec
                    (Pine_kernel.int_add [ offset, bytesConsumed ])
                    blob
                    (char :: chars)


        decodeUtf8Char : Int -> Int -> ( Int, Int )
        decodeUtf8Char blob offset =
            let
                firstByte =
                    Pine_kernel.take [ 1, Pine_kernel.skip [ offset, blob ] ]

                firstByteInt =
                    Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], firstByte ]
            in
            if Pine_kernel.int_is_sorted_asc [ firstByteInt, 0x7F ] then
                -- 1-byte character (ASCII)
                ( firstByte, 1 )

            else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xE0 ], 0xC0 ] then
                -- 2-byte character
                let
                    byte2 =
                        Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

                    byte2Int =
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

                    firstFiveBits =
                        Pine_kernel.bit_and [ firstByteInt, 0x1F ]

                    secondSixBits =
                        Pine_kernel.bit_and [ byte2Int, 0x3F ]

                    charCode =
                        Pine_kernel.int_add
                            [ Pine_kernel.int_mul [ firstFiveBits, 64 ] -- Multiply by 2^6
                            , secondSixBits
                            ]
                in
                ( Pine_kernel.skip [ 1, charCode ], 2 )

            else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xF0 ], 0xE0 ] then
                -- 3-byte character
                let
                    byte2 =
                        Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

                    byte2Int =
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

                    byte3 =
                        Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 2 ], blob ] ]

                    byte3Int =
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte3 ]

                    firstFourBits =
                        Pine_kernel.bit_and [ firstByteInt, 0x0F ]

                    secondSixBits =
                        Pine_kernel.bit_and [ byte2Int, 0x3F ]

                    thirdSixBits =
                        Pine_kernel.bit_and [ byte3Int, 0x3F ]

                    charCode =
                        Pine_kernel.int_add
                            [ Pine_kernel.int_mul [ firstFourBits, 4096 ] -- Multiply by 2^12
                            , Pine_kernel.int_mul [ secondSixBits, 64 ] -- Multiply by 2^6
                            , thirdSixBits
                            ]
                in
                ( Pine_kernel.skip [ 1, charCode ], 3 )

            else if Pine_kernel.equal [ Pine_kernel.bit_and [ firstByteInt, 0xF8 ], 0xF0 ] then
                -- 4-byte character
                let
                    byte2 =
                        Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 1 ], blob ] ]

                    byte2Int =
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte2 ]

                    byte3 =
                        Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 2 ], blob ] ]

                    byte3Int =
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte3 ]

                    byte4 =
                        Pine_kernel.take [ 1, Pine_kernel.skip [ Pine_kernel.int_add [ offset, 3 ], blob ] ]

                    byte4Int =
                        Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], byte4 ]

                    firstThreeBits =
                        Pine_kernel.bit_and [ firstByteInt, 0x07 ]

                    secondSixBits =
                        Pine_kernel.bit_and [ byte2Int, 0x3F ]

                    thirdSixBits =
                        Pine_kernel.bit_and [ byte3Int, 0x3F ]

                    fourthSixBits =
                        Pine_kernel.bit_and [ byte4Int, 0x3F ]

                    charCode =
                        Pine_kernel.int_add
                            [ Pine_kernel.int_mul [ firstThreeBits, 262144 ] -- Multiply by 2^18
                            , Pine_kernel.int_mul [ secondSixBits, 4096 ] -- Multiply by 2^12
                            , Pine_kernel.int_mul [ thirdSixBits, 64 ] -- Multiply by 2^6
                            , fourthSixBits
                            ]
                in
                ( Pine_kernel.skip [ 1, charCode ], 4 )

            else
                -- Invalid UTF-8 sequence; use replacement character
                ( Pine_kernel.skip [ 1, 0xFFFD ], 1 )

         * */

        var argOffsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var blobValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var charsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (charsValue is not PineValue.ListValue charsList)
        {
            return null;
        }

        if (KernelFunction.SignedIntegerFromValueRelaxed(argOffsetValue) is not { } argOffset)
        {
            return null;
        }

        if (blobValue is not PineValue.BlobValue blobUtf8)
        {
            return null;
        }

        var concatBufferInt32 = new int[blobUtf8.Bytes.Length];

        for (var i = 0; i < charsList.Items.Length; ++i)
        {
            if (IntegerEncoding.ParseUnsignedInteger(charsList.Items.Span[i]).IsOkOrNullable() is not { } charCode)
            {
                return null;
            }

            concatBufferInt32[charsList.Items.Length - i - 1] = (int)charCode;
        }

        var sourceOffsetInt = (int)argOffset;

        var writtenChars = charsList.Items.Length;

        while (sourceOffsetInt < blobUtf8.Bytes.Length)
        {
            var byteValue = blobUtf8.Bytes.Span[sourceOffsetInt];

            if (byteValue <= 0x7f)
            {
                // 1-byte character (ASCII)

                concatBufferInt32[writtenChars++] = byteValue;
                sourceOffsetInt += 1;
            }
            else if ((byteValue & 0xE0) == 0xC0)
            {
                // 2-byte character

                if (sourceOffsetInt + 1 >= blobUtf8.Bytes.Length)
                {
                    break; // Not enough bytes for a 2-byte character
                }

                var secondByte = blobUtf8.Bytes.Span[sourceOffsetInt + 1];

                var charCode =
                    ((byteValue & 0x1F) << 6) |
                    (secondByte & 0x3F);

                concatBufferInt32[writtenChars++] = charCode;
                sourceOffsetInt += 2;
            }
            else if ((byteValue & 0xF0) == 0xE0)
            {
                // 3-byte character

                if (sourceOffsetInt + 2 >= blobUtf8.Bytes.Length)
                {
                    break; // Not enough bytes for a 3-byte character
                }

                var secondByte = blobUtf8.Bytes.Span[sourceOffsetInt + 1];
                var thirdByte = blobUtf8.Bytes.Span[sourceOffsetInt + 2];

                var charCode =
                    ((byteValue & 0x0F) << 12) |
                    ((secondByte & 0x3F) << 6) |
                    (thirdByte & 0x3F);

                concatBufferInt32[writtenChars++] = charCode;
                sourceOffsetInt += 3;
            }
            else if ((byteValue & 0xF8) == 0xF0)
            {
                // 4-byte character

                if (sourceOffsetInt + 3 >= blobUtf8.Bytes.Length)
                {
                    break; // Not enough bytes for a 4-byte character
                }

                var secondByte = blobUtf8.Bytes.Span[sourceOffsetInt + 1];
                var thirdByte = blobUtf8.Bytes.Span[sourceOffsetInt + 2];
                var fourthByte = blobUtf8.Bytes.Span[sourceOffsetInt + 3];

                var charCode =
                    ((byteValue & 0x07) << 18) |
                    ((secondByte & 0x3F) << 12) |
                    ((thirdByte & 0x3F) << 6) |
                    (fourthByte & 0x3F);

                concatBufferInt32[writtenChars++] = charCode;
                sourceOffsetInt += 4;
            }
            else
            {
                // Invalid UTF-8 sequence; use replacement character

                concatBufferInt32[writtenChars++] = 0xFFFD;
                sourceOffsetInt += 1; // Skip one byte
            }
        }

        var concatBuffer = new byte[writtenChars * 4];

        for (var i = 0; i < writtenChars; ++i)
        {
            BinaryPrimitives.WriteInt32BigEndian(concatBuffer.AsSpan(i * 4), concatBufferInt32[i]);
        }

        var elmStringValue =
            PineValue.List(
                [
                ElmValue.ElmStringTypeTagNameAsValue,
                PineValue.List([PineValue.Blob(concatBuffer)])
                ]);

        return new PrecompiledResult.FinalValue(elmStringValue, 0);
    }

    static PrecompiledResult.FinalValue? BytesEncode_encodeBlob(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
         * 
        encodeBlob : Encoder -> Int
        encodeBlob builder =
            case builder of
                I8 n ->
                    if Pine_kernel.int_is_sorted_asc [ 0, n ] then
                        Pine_kernel.take [ 1, Pine_kernel.reverse n ]

                    else
                        Pine_kernel.take
                            [ 1
                            , Pine_kernel.reverse
                                (Pine_kernel.bit_not (Pine_kernel.int_add [ n, 1 ]))
                            ]

                I16 e n ->
                    let
                        littleEndian =
                            if Pine_kernel.int_is_sorted_asc [ 0, n ] then
                                Pine_kernel.take
                                    [ 2
                                    , Pine_kernel.concat
                                        [ Pine_kernel.reverse
                                            (Pine_kernel.skip [ 1, n ])
                                        , Pine_kernel.skip [ 1, 0 ]
                                        ]
                                    ]

                            else
                                Pine_kernel.take
                                    [ 2
                                    , Pine_kernel.concat
                                        [ Pine_kernel.reverse
                                            (Pine_kernel.skip
                                                [ 1
                                                , Pine_kernel.bit_not (Pine_kernel.int_add [ n, 1 ])
                                                ]
                                            )
                                        , Pine_kernel.skip [ 1, 0xFF ]
                                        ]
                                    ]
                    in
                    if Pine_kernel.equal [ e, Bytes.LE ] then
                        littleEndian

                    else
                        Pine_kernel.reverse littleEndian

                I32 e n ->
                    let
                        littleEndian =
                            if Pine_kernel.int_is_sorted_asc [ 0, n ] then
                                Pine_kernel.take
                                    [ 4
                                    , Pine_kernel.concat
                                        [ Pine_kernel.reverse
                                            (Pine_kernel.skip [ 1, n ])
                                        , Pine_kernel.skip [ 2, 0x01000000 ]
                                        ]
                                    ]

                            else
                                Pine_kernel.take
                                    [ 4
                                    , Pine_kernel.concat
                                        [ Pine_kernel.reverse
                                            (Pine_kernel.skip
                                                [ 1
                                                , Pine_kernel.bit_not (Pine_kernel.int_add [ n, 1 ])
                                                ]
                                            )
                                        , Pine_kernel.skip [ 1, 0x00FFFFFF ]
                                        ]
                                    ]
                    in
                    if Pine_kernel.equal [ e, Bytes.LE ] then
                        littleEndian

                    else
                        Pine_kernel.reverse littleEndian

                U8 n ->
                    Pine_kernel.take [ 1, Pine_kernel.reverse n ]

                U16 e n ->
                    let
                        littleEndian =
                            Pine_kernel.take
                                [ 2
                                , Pine_kernel.concat
                                    [ Pine_kernel.reverse
                                        (Pine_kernel.skip [ 1, n ])
                                    , Pine_kernel.skip [ 1, 0 ]
                                    ]
                                ]
                    in
                    if Pine_kernel.equal [ e, Bytes.LE ] then
                        littleEndian

                    else
                        Pine_kernel.reverse littleEndian

                U32 e n ->
                    let
                        littleEndian =
                            Pine_kernel.take
                                [ 4
                                , Pine_kernel.concat
                                    [ Pine_kernel.reverse
                                        (Pine_kernel.skip [ 1, n ])
                                    , Pine_kernel.skip [ 2, 0x01000000 ]
                                    ]
                                ]
                    in
                    if Pine_kernel.equal [ e, Bytes.LE ] then
                        littleEndian

                    else
                        Pine_kernel.reverse littleEndian

                SequenceEncoder bs ->
                    if Pine_kernel.equal [ bs, [] ] then
                        Pine_kernel.take [ 0, 0 ]

                    else
                        Pine_kernel.concat (List.map encodeBlob bs)

                BytesEncoder (Bytes.Elm_Bytes blob) ->
                    blob
         * */

        var chunks = new List<ReadOnlyMemory<byte>>();

        void collectChunks(PineValue argBuilder)
        {
            var argBuilderTagValue =
                PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [0]);

            if (argBuilderTagValue == Tag_I8_Name_Value)
            {
                var nValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 0]);

                if (IntegerEncoding.ParseSignedIntegerRelaxed(nValue).IsOkOrNullable() is not { } n)
                {
                    return;
                }

                /*
                var wrapped = (byte)(int)n;

                if (wrapped < 0)
                {
                    wrapped = (byte)(256 + wrapped);
                }

                chunks.Add(PineValue.BlobSingleByte(wrapped).Bytes);
                */

                var asByte = (byte)(n & 0xff);

                chunks.Add(PineValue.BlobSingleByte(asByte).Bytes);

                return;
            }

            if (argBuilderTagValue == Tag_I16_Name_Value)
            {
                var eValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 0]);

                var nValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 1]);

                if (IntegerEncoding.ParseSignedIntegerRelaxed(nValue).IsOkOrNullable() is not { } n)
                {
                    return;
                }

                var littleEndian = eValue == Tag_LE_Value;

                var bytes = new byte[2];

                if (littleEndian)
                {
                    bytes[0] = (byte)(n & 0xFF);
                    bytes[1] = (byte)((n >> 8) & 0xFF);
                }
                else
                {
                    bytes[0] = (byte)((n >> 8) & 0xFF);
                    bytes[1] = (byte)(n & 0xFF);
                }

                chunks.Add(new ReadOnlyMemory<byte>(bytes));

                return;
            }

            if (argBuilderTagValue == Tag_I32_Name_Value)
            {
                var eValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 0]);

                var nValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 1]);

                if (IntegerEncoding.ParseSignedIntegerRelaxed(nValue).IsOkOrNullable() is not { } n)
                {
                    return;
                }

                var littleEndian = eValue == Tag_LE_Value;
                var bytes = new byte[4];

                if (littleEndian)
                {
                    bytes[0] = (byte)(n & 0xFF);
                    bytes[1] = (byte)((n >> 8) & 0xFF);
                    bytes[2] = (byte)((n >> 16) & 0xFF);
                    bytes[3] = (byte)((n >> 24) & 0xFF);
                }
                else
                {
                    bytes[0] = (byte)((n >> 24) & 0xFF);
                    bytes[1] = (byte)((n >> 16) & 0xFF);
                    bytes[2] = (byte)((n >> 8) & 0xFF);
                    bytes[3] = (byte)(n & 0xFF);
                }

                chunks.Add(new ReadOnlyMemory<byte>(bytes));

                return;
            }

            if (argBuilderTagValue == Tag_U8_Name_Value)
            {
                var nValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 0]);

                if (IntegerEncoding.ParseSignedIntegerRelaxed(nValue).IsOkOrNullable() is not { } n)
                {
                    return;
                }

                var asByte = (byte)(n & 0xff);

                chunks.Add(PineValue.BlobSingleByte(asByte).Bytes);

                return;
            }

            if (argBuilderTagValue == Tag_U16_Name_Value)
            {
                var eValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 0]);

                var nValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 1]);

                if (IntegerEncoding.ParseSignedIntegerRelaxed(nValue).IsOkOrNullable() is not { } n)
                {
                    return;
                }

                var littleEndian = eValue == Tag_LE_Value;
                var bytes = new byte[2];

                if (littleEndian)
                {
                    bytes[0] = (byte)(n & 0xFF);
                    bytes[1] = (byte)((n >> 8) & 0xFF);
                }
                else
                {
                    bytes[0] = (byte)((n >> 8) & 0xFF);
                    bytes[1] = (byte)(n & 0xFF);
                }

                chunks.Add(new ReadOnlyMemory<byte>(bytes));

                return;
            }

            if (argBuilderTagValue == Tag_U32_Name_Value)
            {
                var eValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 0]);

                var nValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 1]);

                if (IntegerEncoding.ParseSignedIntegerRelaxed(nValue).IsOkOrNullable() is not { } n)
                {
                    return;
                }

                var littleEndian = eValue == Tag_LE_Value;
                var bytes = new byte[4];

                if (littleEndian)
                {
                    bytes[0] = (byte)(n & 0xFF);
                    bytes[1] = (byte)((n >> 8) & 0xFF);
                    bytes[2] = (byte)((n >> 16) & 0xFF);
                    bytes[3] = (byte)((n >> 24) & 0xFF);
                }
                else
                {
                    bytes[0] = (byte)((n >> 24) & 0xFF);
                    bytes[1] = (byte)((n >> 16) & 0xFF);
                    bytes[2] = (byte)((n >> 8) & 0xFF);
                    bytes[3] = (byte)(n & 0xFF);
                }

                chunks.Add(new ReadOnlyMemory<byte>(bytes));

                return;
            }

            if (argBuilderTagValue == Tag_BytesEncoder_Name_Value)
            {
                var bytesBlobValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 0, 1, 0]);

                if (bytesBlobValue is PineValue.BlobValue bytesBlob)
                {
                    chunks.Add(bytesBlob.Bytes);
                }

                return;
            }

            if (argBuilderTagValue == Tag_SequenceEncoder_Name_Value)
            {
                var sequenceValue =
                    PineVM.ValueFromPathInValueOrEmptyList(argBuilder, [1, 0]);

                if (sequenceValue is PineValue.ListValue sequenceList)
                {
                    for (var i = 0; i < sequenceList.Items.Length; ++i)
                    {
                        collectChunks(sequenceList.Items.Span[i]);
                    }
                }

                return;
            }

            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        var argBuilder =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        collectChunks(argBuilder);

        var concatenated = BytesConversions.Concat(chunks);

        return new PrecompiledResult.FinalValue(PineValue.Blob(concatenated), 0);
    }

    private static PrecompiledResult.FinalValue? Danfishgold_Base64_Encode_toBytes(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
         * 
         * 
            toBytes : String -> Maybe Bytes
            toBytes string =
                Maybe.map Encode.encode (encoder string)
         * */

        var stringArg =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        if (stringArg is not PineValue.ListValue stringList)
        {
            return null;
        }

        if (stringList.Items.Length is not 2)
        {
            return null;
        }

        var stringTagName = stringList.Items.Span[0];

        if (stringTagName != ElmValue.ElmStringTypeTagNameAsValue)
        {
            return null;
        }

        if (stringList.Items.Span[1] is not PineValue.ListValue stringTagArguments)
        {
            return null;
        }

        if (stringTagArguments.Items.Length is not 1)
        {
            return null;
        }

        if (stringTagArguments.Items.Span[0] is not PineValue.BlobValue stringCharsBlob)
        {
            return null;
        }

        var asStringResult = StringEncoding.StringFromValue(stringCharsBlob);

        if (asStringResult.IsOkOrNull() is not { } dotnetString)
        {
            return null;
        }

        var dotnetBytesBuffer = new byte[dotnetString.Length];

        if (!Convert.TryFromBase64String(dotnetString, dotnetBytesBuffer, out var bytesWritten))
        {
            // Case of an invalid base64 string

            return new PrecompiledResult.FinalValue(Tag_Nothing_Value, 0);
        }

        var bytesValue =
            PineValue.List(
            [
                ElmValue.ElmBytesTypeTagNameAsValue,
                PineValue.List([PineValue.Blob(dotnetBytesBuffer.AsMemory(start:0, length: bytesWritten))])
            ]);

        var maybeBytesValue =
            PineValue.List(
                [
                    Tag_Just_Name_Value,
                    PineValue.List([bytesValue])
                ]);

        return new PrecompiledResult.FinalValue(maybeBytesValue, 0);
    }


    private static PrecompiledResult.FinalValue? Danfishgold_Base64_Decode_fromBytes(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
         * 
        fromBytes : Bytes -> Maybe String
        fromBytes bytes =
            Decode.decode (decoder (Bytes.width bytes)) bytes


        decoder : Int -> Decode.Decoder String
        decoder width =
            Decode.loop { remaining = width, string = "" } loopHelp

         * */


        var argBytes =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        if (argBytes is not PineValue.ListValue argBytesList)
        {
            return null;
        }

        if (argBytesList.Items.Length is not 2)
        {
            return null;
        }

        if (argBytesList.Items.Span[0] != ElmValue.ElmBytesTypeTagNameAsValue)
        {
            return null;
        }

        if (argBytesList.Items.Span[1] is not PineValue.ListValue argBytesTagArguments)
        {
            return null;
        }

        if (argBytesTagArguments.Items.Length is not 1)
        {
            return null;
        }

        if (argBytesTagArguments.Items.Span[0] is not PineValue.BlobValue argBytesBlobValue)
        {
            return null;
        }

        var dotnetString = Convert.ToBase64String(argBytesBlobValue.Bytes.Span);

        var stringValue =
            ElmValueEncoding.StringAsPineValue(dotnetString);

        var maybeStringValue =
            PineValue.List(
                [
                    Tag_Just_Name_Value,
                    PineValue.List([stringValue])
                ]);

        return new PrecompiledResult.FinalValue(maybeStringValue, 0);
    }

    static PineValue BasicsCompare(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argA = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var argB = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return BasicsCompare(argA, argB);
    }

    static PineValue BasicsCompare(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return Tag_EQ_Value;
        }

        var aTag = PineVM.ValueFromPathInValueOrEmptyList(a, [0]);
        var bTag = PineVM.ValueFromPathInValueOrEmptyList(b, [0]);

        if (aTag == ElmValue.ElmStringTypeTagNameAsValue && bTag == ElmValue.ElmStringTypeTagNameAsValue)
        {
            return
                CompareStrings(
                    PineVM.ValueFromPathInValueOrEmptyList(a, [1, 0]),
                    PineVM.ValueFromPathInValueOrEmptyList(b, [1, 0]));
        }

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue && bTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var aTagArgs = PineVM.ValueFromPathInValueOrEmptyList(a, [1]);
            var bTagArgs = PineVM.ValueFromPathInValueOrEmptyList(b, [1]);

            var numA = PineVM.ValueFromPathInValueOrEmptyList(aTagArgs, [0]);
            var denomA = PineVM.ValueFromPathInValueOrEmptyList(aTagArgs, [1]);

            var numB = PineVM.ValueFromPathInValueOrEmptyList(bTagArgs, [0]);
            var denomB = PineVM.ValueFromPathInValueOrEmptyList(bTagArgs, [1]);

            var leftProduct = KernelFunctionSpecialized.int_mul(numA, denomB);
            var rightProduct = KernelFunctionSpecialized.int_mul(numB, denomA);

            if (leftProduct == rightProduct)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([leftProduct, rightProduct])) == PineKernelValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var aTagArgs = PineVM.ValueFromPathInValueOrEmptyList(a, [1]);

            var numA = PineVM.ValueFromPathInValueOrEmptyList(aTagArgs, [0]);
            var denomA = PineVM.ValueFromPathInValueOrEmptyList(aTagArgs, [1]);

            var rightProduct = KernelFunctionSpecialized.int_mul(denomA, b);

            if (numA == rightProduct)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([numA, rightProduct])) == PineKernelValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (bTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var bTagArgs = PineVM.ValueFromPathInValueOrEmptyList(b, [1]);

            var numB = PineVM.ValueFromPathInValueOrEmptyList(bTagArgs, [0]);
            var denomB = PineVM.ValueFromPathInValueOrEmptyList(bTagArgs, [1]);

            var leftProduct = KernelFunctionSpecialized.int_mul(a, denomB);

            if (leftProduct == numB)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([leftProduct, numB])) == PineKernelValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (a is PineValue.ListValue)
        {
            return CompareLists(a, b);
        }

        if (KernelFunction.int_is_sorted_asc(PineValue.List([a, b])) == PineKernelValues.TrueValue)
        {
            return Tag_LT_Value;
        }

        return Tag_GT_Value;
    }

    static PineValue CompareLists(PineValue a, PineValue b)
    {
        if (a == PineValue.EmptyList)
        {
            if (b == PineValue.EmptyList)
            {
                return Tag_EQ_Value;
            }

            return Tag_LT_Value;
        }

        if (a is PineValue.ListValue listA && 0 < listA.Items.Length)
        {
            if (b == PineValue.EmptyList)
            {
                return Tag_GT_Value;
            }

            if (b is PineValue.ListValue listB && 0 < listB.Items.Length)
            {
                var commonLength =
                    listA.Items.Length < listB.Items.Length ?
                    listA.Items.Length :
                    listB.Items.Length;

                for (var i = 0; i < commonLength; ++i)
                {
                    var itemA = listA.Items.Span[i];
                    var itemB = listB.Items.Span[i];

                    var itemOrder = BasicsCompare(itemA, itemB);

                    if (itemOrder != Tag_EQ_Value)
                    {
                        return itemOrder;
                    }
                }

                if (listA.Items.Length < listB.Items.Length)
                {
                    return Tag_LT_Value;
                }

                if (listA.Items.Length > listB.Items.Length)
                {
                    return Tag_GT_Value;
                }

                return Tag_EQ_Value;
            }

            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PineValue CompareStrings(PineValue stringA, PineValue stringB)
    {
        /*
        compareStrings : Int -> Int -> Int -> Order
        compareStrings offset stringA stringB =
            let
                charA =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offset, stringA ]
                        ]

                charB =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offset, stringB ]
                        ]
            in
            if Pine_kernel.equal [ Pine_kernel.length charA, 0 ] then
                if Pine_kernel.equal [ Pine_kernel.length charB, 0 ] then
                    EQ

                else
                    LT

            else if Pine_kernel.equal [ Pine_kernel.length charB, 0 ] then
                GT

            else if Pine_kernel.equal [ charA, charB ] then
                compareStrings
                    (Pine_kernel.int_add [ offset, 4 ])
                    stringA
                    stringB

            else if
                -- Pine_kernel.int_is_sorted_asc only works with signed integers. Therefore prepend sign to each character.
                Pine_kernel.int_is_sorted_asc
                    [ Pine_kernel.concat [ 0, charA ]
                    , Pine_kernel.concat [ 0, charB ]
                    ]
            then
                LT

            else
                GT
         * */

        if (stringA is PineValue.BlobValue blobA && stringB is PineValue.BlobValue blobB)
        {
            var commonLength =
                blobA.Bytes.Length < blobB.Bytes.Length ?
                blobA.Bytes.Length :
                blobB.Bytes.Length;

            var commonLenghtChars = commonLength / 4;

            for (var i = 0; i < commonLenghtChars; ++i)
            {
                var offset = i * 4;

                var charA = BinaryPrimitives.ReadInt32BigEndian(blobA.Bytes.Span[offset..]);
                var charB = BinaryPrimitives.ReadInt32BigEndian(blobB.Bytes.Span[offset..]);

                if (charA == charB)
                {
                    continue;
                }

                return charA < charB ? Tag_LT_Value : Tag_GT_Value;
            }

            return
                blobA.Bytes.Length < blobB.Bytes.Length
                ? Tag_LT_Value
                : blobA.Bytes.Length > blobB.Bytes.Length
                ? Tag_GT_Value
                : Tag_EQ_Value;
        }

        return
            stringA == stringB
            ? Tag_EQ_Value
            : KernelFunction.int_is_sorted_asc(PineValue.List([stringA, stringB])) == PineKernelValues.TrueValue
            ? Tag_LT_Value
            : Tag_GT_Value;
    }

    static PrecompiledResult.FinalValue BasicsEq(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argA = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var argB = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return BasicsEq(argA, argB);
    }

    static PrecompiledResult.FinalValue BasicsEq(PineValue a, PineValue b)
    {
        var (isEq, stackFrameCount) = BasicsEqRecursive(a, b);

        return new PrecompiledResult.FinalValue(
            isEq ? PineKernelValues.TrueValue : PineKernelValues.FalseValue,
            StackFrameCount: stackFrameCount);
    }

    static (bool, int) BasicsEqRecursive(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return (true, 0);
        }

        var aTag = PineVM.ValueFromPathInValueOrEmptyList(a, [0]);
        var bTag = PineVM.ValueFromPathInValueOrEmptyList(b, [0]);

        var aTagArgs = PineVM.ValueFromPathInValueOrEmptyList(a, [1]);
        var bTagArgs = PineVM.ValueFromPathInValueOrEmptyList(b, [1]);

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue &&
            aTagArgs is PineValue.ListValue argsList && argsList.Items.Length is 2)
        {
            var numAValue = argsList.Items.Span[0];
            var denAValue = argsList.Items.Span[1];

            return (numAValue == b && denAValue == IntegerOneValue, 0);
        }

        if (bTag == ElmValue.ElmFloatTypeTagNameAsValue &&
            bTagArgs is PineValue.ListValue argsListB && argsListB.Items.Length is 2)
        {
            var numBValue = argsListB.Items.Span[0];
            var denBValue = argsListB.Items.Span[1];

            return (a == numBValue && IntegerOneValue == denBValue, 0);
        }

        if (a is PineValue.BlobValue)
        {
            return (false, 0);
        }

        {
            if (a is PineValue.ListValue listA && b is PineValue.ListValue listB)
            {
                if (listA.Items.Length != listB.Items.Length)
                {
                    return (false, 0);
                }

                if (aTag == ElmValue.ElmStringTypeTagNameAsValue)
                {
                    return (false, 0);
                }

                if (aTag == ElmValue.ElmDictNotEmptyTagNameAsValue)
                {
                    var dictAList = DictToListRecursive(a);
                    var dictBList = DictToListRecursive(b);

                    return
                        (PineValue.List(dictAList) == PineValue.List(dictBList), dictAList.Length + dictBList.Length);
                }

                if (aTag == ElmValue.ElmSetTypeTagNameAsValue)
                {
                    var dictA = PineVM.ValueFromPathInValueOrEmptyList(a, [1, 0]);
                    var dictB = PineVM.ValueFromPathInValueOrEmptyList(b, [1, 0]);

                    var dictAKeys = DictKeysRecursive(dictA);
                    var dictBKeys = DictKeysRecursive(dictB);

                    return
                        (PineValue.List(dictAKeys) == PineValue.List(dictBKeys), dictAKeys.Length + dictBKeys.Length);
                }

                return ListsEqualRecursive(listA.Items, listB.Items);
            }
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    public static ReadOnlyMemory<PineValue> DictToListRecursive(PineValue dict)
    {
        var tag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

        if (tag == ElmValue.ElmDictEmptyTagNameAsValue)
        {
            return ReadOnlyMemory<PineValue>.Empty;
        }

        if (tag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

            var argKey = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [1]);
            var argValue = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [2]);
            var argLeft = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
            var argRight = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

            var fromLeft = DictToListRecursive(argLeft);
            var fromRight = DictToListRecursive(argRight);

            var result = new PineValue[fromLeft.Length + fromRight.Length + 1];

            fromLeft.Span.CopyTo(result);

            result[fromLeft.Length] = PineValue.List([argKey, argValue]);

            fromRight.Span.CopyTo(result.AsSpan(fromLeft.Length + 1));

            return result;
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static ReadOnlyMemory<PineValue> DictKeysRecursive(PineValue dict)
    {
        var tag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

        if (tag == ElmValue.ElmDictEmptyTagNameAsValue)
        {
            return ReadOnlyMemory<PineValue>.Empty;
        }

        if (tag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

            var argKey = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [1]);

            var argLeft = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
            var argRight = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

            var fromLeft = DictKeysRecursive(argLeft);
            var fromRight = DictKeysRecursive(argRight);

            var result = new PineValue[fromLeft.Length + fromRight.Length + 1];

            fromLeft.Span.CopyTo(result);

            result[fromLeft.Length] = argKey;

            fromRight.Span.CopyTo(result.AsSpan(fromLeft.Length + 1));

            return result;
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static (bool, int) ListsEqualRecursive(
        ReadOnlyMemory<PineValue> listA,
        ReadOnlyMemory<PineValue> listB)
    {
        var totalCount = 0;

        for (var i = 0; i < listA.Length; i++)
        {
            var (itemEq, itemCount) = BasicsEqRecursive(listA.Span[i], listB.Span[i]);

            if (!itemEq)
            {
                return (false, totalCount + itemCount);
            }

            totalCount += itemCount + 1;
        }

        return (true, totalCount);
    }

    static PrecompiledResult.FinalValue ListMember(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var item = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var list = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return ListMember(item, list);
    }

    static PrecompiledResult.FinalValue ListMember(PineValue item, PineValue list)
    {
        if (list is PineValue.ListValue listValue)
        {
            return ListMember(item, listValue.Items);
        }

        return new PrecompiledResult.FinalValue(
            PineKernelValues.FalseValue,
            StackFrameCount: 0);
    }

    static PrecompiledResult.FinalValue ListMember(PineValue item, ReadOnlyMemory<PineValue> list)
    {
        var totalCount = 0;

        for (var i = 0; i < list.Length; ++i)
        {
            var (itemEq, itemEqStackFrameCount) = BasicsEqRecursive(item, list.Span[i]);

            totalCount += itemEqStackFrameCount;

            if (itemEq)
            {
                return new PrecompiledResult.FinalValue(
                    PineKernelValues.TrueValue,
                    StackFrameCount: totalCount);
            }
        }

        return new PrecompiledResult.FinalValue(
            PineKernelValues.FalseValue,
            StackFrameCount: totalCount);
    }

    static PrecompiledResult.FinalValue CommonAssocListGet(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var key = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var list = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return CommonAssocListGetRecursive(key, list);
    }

    static PrecompiledResult.FinalValue CommonAssocListGetRecursive(
        PineValue key,
        PineValue list)
    {
        if (list is PineValue.ListValue listValue)
        {
            var totalCount = 0;

            for (var i = 0; i < listValue.Items.Length; ++i)
            {
                if (listValue.Items.Span[i] is PineValue.ListValue itemList && 1 < itemList.Items.Length)
                {
                    var (itemEq, itemEqStackFrameCount) =
                        BasicsEqRecursive(key, itemList.Items.Span[0]);

                    totalCount += itemEqStackFrameCount;

                    if (itemEq)
                    {
                        return new PrecompiledResult.FinalValue(
                            Tag_Just_Value(itemList.Items.Span[1]),
                            StackFrameCount: totalCount);
                    }
                }
            }
        }

        return new PrecompiledResult.FinalValue(
            Tag_Nothing_Value,
            StackFrameCount: 0);
    }

    static PrecompiledResult.FinalValue CommonAssocListGetWithIndexHelper(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var index =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var key =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var list =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (list is not PineValue.ListValue listValue)
        {
            return new PrecompiledResult.FinalValue(
                Tag_Nothing_Value,
                StackFrameCount: 0);
        }

        for (var i = 0; i < listValue.Items.Length; ++i)
        {
            if (listValue.Items.Span[i] is PineValue.ListValue itemList)
            {
                var candidateKey =
                    itemList.Items.Length < 1
                    ?
                    PineValue.EmptyList
                    :
                    itemList.Items.Span[0];

                var (itemEq, itemEqStackFrameCount) =
                    BasicsEqRecursive(key, candidateKey);

                if (itemEq)
                {
                    var itemListValue =
                        itemList.Items.Length < 2
                        ?
                        PineValue.EmptyList
                        :
                        itemList.Items.Span[1];

                    return
                        new PrecompiledResult.FinalValue(
                            Tag_Just_Value(
                                PineValue.List(
                                [
                                    KernelFunctionSpecialized.int_add(index, IntegerEncoding.EncodeSignedInteger(i)),
                                    itemListValue
                                ])),
                        StackFrameCount: itemEqStackFrameCount);
                }
            }
        }

        return new PrecompiledResult.FinalValue(
            Tag_Nothing_Value,
            StackFrameCount: 0);
    }


    static PrecompiledResult.FinalValue CommonListUniqueHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var remaining =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var accumulator =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (remaining is not PineValue.ListValue remainingList)
        {
            return new PrecompiledResult.FinalValue(
                accumulator,
                StackFrameCount: 0);

        }

        if (accumulator is not PineValue.ListValue accumulatorList)
        {
            return new PrecompiledResult.FinalValue(
                accumulator,
                StackFrameCount: 0);
        }

        var newUnique = new List<PineValue>(accumulatorList.Items.ToArray());

        for (var i = 0; i < remainingList.Items.Length; ++i)
        {
            var item = remainingList.Items.Span[i];

            var (isMember, _) = ListMember(item, newUnique.ToArray());

            if (isMember != PineKernelValues.TrueValue)
            {
                newUnique.Add(item);
            }
        }

        return new PrecompiledResult.FinalValue(
            PineValue.List([.. accumulatorList.Items.ToArray(), .. newUnique]),
            StackFrameCount: 0);
    }

    static PineValue DictGet(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var targetKey = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var dict = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return DictGet(targetKey, dict);
    }

    static PineValue DictGet(PineValue targetKey, PineValue dict)
    {
        var dictTag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

        if (dictTag == ElmValue.ElmDictEmptyTagNameAsValue)
        {
            return Tag_Nothing_Value;
        }

        if (dictTag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

            var key = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [1]);
            var value = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [2]);
            var left = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
            var right = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

            var comparison = BasicsCompare(targetKey, key);

            if (comparison == Tag_LT_Value)
            {
                return DictGet(targetKey, left);
            }

            if (comparison == Tag_EQ_Value)
            {
                return Tag_Just_Value(value);
            }

            if (comparison == Tag_GT_Value)
            {
                return DictGet(targetKey, right);
            }
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PineValue DictSizeHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        static long sizeHelp(
            long n,
            PineValue dict)
        {
            var dictTag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

            if (dictTag == ElmValue.ElmDictEmptyTagNameAsValue)
            {
                return n;
            }

            if (dictTag == ElmValue.ElmDictNotEmptyTagNameAsValue)
            {
                var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

                var left = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
                var right = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

                return sizeHelp(sizeHelp(n + 1, right), left);
            }

            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        var countValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var count =
            IntegerEncoding.ParseSignedIntegerStrict(countValue)
            .Extract(err => throw new Exception(err));

        var dict = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return IntegerEncoding.EncodeSignedInteger(sizeHelp((long)count, dict));
    }


    static PineValue ElmCompiledRecordAccess(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var fieldNameValue = PineVM.ValueFromPathInValueOrEmptyList(environment, [1]);
        var remainingFieldsValue = PineVM.ValueFromPathInValueOrEmptyList(environment, [2]);

        if (remainingFieldsValue is PineValue.ListValue remainingFields)
        {
            for (var i = 0; i < remainingFields.Items.Length; ++i)
            {
                var field = remainingFields.Items.Span[i];

                if (field is PineValue.ListValue fieldList && 0 < fieldList.Items.Length)
                {
                    if (fieldList.Items.Span[0] == fieldNameValue)
                    {
                        if (1 < fieldList.Items.Length)
                        {
                            return fieldList.Items.Span[1];
                        }

                        return PineValue.EmptyList;
                    }
                }
            }
        }

        throw new ParseExpressionException("invalid record access - field name not found");
    }

    static PrecompiledResult.FinalValue CountPineValueContent(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var valueArgument =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var (nodeCount, byteCount) =
            CountEncodedPineValueContentRecursive(valueArgument);

        var returnValue =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(nodeCount),
                IntegerEncoding.EncodeSignedInteger(byteCount)
                ]);

        return
            new PrecompiledResult.FinalValue(
                returnValue,
                StackFrameCount: nodeCount);
    }

    static readonly ConcurrentDictionary<PineValue, (long nodeCount, long byteCount)> countEncodedPineValueContentDict = new();

    static (long nodeCount, long byteCount) CountEncodedPineValueContentRecursive(
        PineValue encodedValue)
    {
        return
            countEncodedPineValueContentDict.GetOrAdd(
            encodedValue,
            valueFactory: ComputeCountEncodedPineValueContentRecursive);
    }

    static (long nodeCount, long byteCount) ComputeCountEncodedPineValueContentRecursive(
        PineValue encodedValue)
    {
        if (encodedValue is not PineValue.ListValue listValue)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        if (listValue.Items.Length is not 2)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        if (listValue.Items.Span[1] is not PineValue.ListValue tagArgumentsList)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        if (tagArgumentsList.Items.Length is not 1)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        var tagArgument = tagArgumentsList.Items.Span[0];

        if (listValue.Items.Span[0] == Tag_ListValue_Value &&
            tagArgument is PineValue.ListValue listValueListList)
        {
            long nodeCount = 0;
            long byteCount = 0;

            for (var i = 0; i < listValueListList.Items.Length; ++i)
            {
                var (itemNodeCount, itemByteCount) =
                    CountEncodedPineValueContentRecursive(listValueListList.Items.Span[i]);

                nodeCount += itemNodeCount + 1;
                byteCount += itemByteCount;
            }

            return (nodeCount, byteCount);
        }

        if (listValue.Items.Span[0] == Tag_BlobValue_Value &&
            tagArgument is PineValue.ListValue blobValueList)
        {
            return (0, blobValueList.Items.Length);
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PrecompiledResult.FinalValue? PineComputeValueFromString_2024_Recursive(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        computeValueFromStringRecursive : List Value -> List Char -> Value
        computeValueFromStringRecursive mappedChars string =
            case string of
                [] ->
                    ListValue (List.reverse mappedChars)

                nextChar :: restOfChars ->
                    computeValueFromStringRecursive (valueFromChar nextChar :: mappedChars) restOfChars


        valueFromChar : Char -> Value
        valueFromChar char =
            BlobValue (unsafeUnsignedBlobValueFromInt (Char.toCode char))

        */

        if (PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]) is not PineValue.ListValue mappedCharsList)
        {
            return null;
        }

        var stringArgument =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (stringArgument is not PineValue.ListValue stringCharsList)
        {
            return null;
        }

        var mappedValues =
            new PineValue[mappedCharsList.Items.Length + stringCharsList.Items.Length];

        for (var i = 0; i < mappedCharsList.Items.Length; ++i)
        {
            mappedValues[i] = mappedCharsList.Items.Span[i];
        }

        for (var i = 0; i < stringCharsList.Items.Length; ++i)
        {
            if (stringCharsList.Items.Span[i] is not PineValue.BlobValue charBlobValue)
            {
                return null;
            }

            var blobValueIntegers = new PineValue[charBlobValue.Bytes.Length];

            for (var j = 0; j < charBlobValue.Bytes.Length; ++j)
            {
                blobValueIntegers[j] = PineValue.Blob([4, charBlobValue.Bytes.Span[j]]);
            }

            var blobValue =
                PineValue.List(
                    [
                    Tag_BlobValue_Value,
                    PineValue.List([PineValue.List(blobValueIntegers)])
                    ]);

            mappedValues[mappedCharsList.Items.Length + i] = blobValue;
        }

        var finalValue =
            PineValue.List(
                [
                Tag_ListValue_Value,
                PineValue.List([PineValue.List(mappedValues)])
                ]);

        return
            new PrecompiledResult.FinalValue(
                finalValue,
                StackFrameCount: 3 + mappedValues.Length * 1);
    }

    static PrecompiledResult.FinalValue? PineComputeValueFromString_2025(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        computeValueFromString_2025 : String -> Value
        computeValueFromString_2025 string =
            let
                charsBytes : List (List Int)
                charsBytes =
                    List.map blobBytesFromChar (String.toList string)
            in
            BlobValue
                (List.concat charsBytes)


        blobBytesFromChar : Char -> List Int
        blobBytesFromChar char =
            let
                charCode : Int
                charCode =
                    Char.toCode char
            in
            [ modBy 0x0100 (charCode // 0x01000000)
            , modBy 0x0100 (charCode // 0x00010000)
            , modBy 0x0100 (charCode // 0x0100)
            , modBy 0x0100 charCode
            ]
        */

        var argStringCharsBlobValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0, 1, 0]);

        if (argStringCharsBlobValue is not PineValue.BlobValue charsBlob)
        {
            return null;
        }

        var blobValueIntegers = new PineValue[charsBlob.Bytes.Length];

        for (var j = 0; j < charsBlob.Bytes.Length; ++j)
        {
            blobValueIntegers[j] = PineValue.Blob([4, charsBlob.Bytes.Span[j]]);
        }

        var blobValue =
            PineValue.List(
                [
                Tag_BlobValue_Value,
                PineValue.List([PineValue.List(blobValueIntegers)])
                ]);

        return
            new PrecompiledResult.FinalValue(
                blobValue,
                StackFrameCount: 3 + charsBlob.Bytes.Length);
    }

    static PrecompiledResult.FinalValue? ElmKernelParser_countOffsetsInString(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        if (IntegerEncoding.ParseSignedIntegerRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0, 0])).IsOkOrNullable() is not { } offset)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0, 1])).IsOkOrNullable() is not { } newlines)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0, 2])).IsOkOrNullable() is not { } col)
        {
            return null;
        }

        if (PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1, 0]) is not PineValue.ListValue charsList)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1, 1])).IsOkOrNullable() is not { } end)
        {
            return null;
        }

        if (offset < 0 || offset >= charsList.Items.Length || end > charsList.Items.Length)
        {
            return null;
        }

        for (var i = (int)offset; i < end; ++i)
        {
            if (charsList.Items.Span[i] == Character_ASCII_Newline_Value)
            {
                ++newlines;
                col = 0;
                continue;
            }

            ++col;
        }

        var finalValue =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(newlines),
                IntegerEncoding.EncodeSignedInteger(col)
                ]);

        return
            new PrecompiledResult.FinalValue(
                finalValue,
                StackFrameCount: 1 + charsList.Items.Length);
    }

    /*

        findSubString : String -> Int -> Int -> Int -> List Char -> ( Int, Int, Int )
        findSubString (String smallChars) offset row col bigChars =
            let
                newOffset : Int
                newOffset =
                    indexOf smallChars bigChars offset

                consumedLength : Int
                consumedLength =
                    Pine_kernel.int_add
                        [ newOffset
                        , Pine_kernel.negate offset
                        ]

                ( newlineCount, colShift ) =
                    countOffsetsInString ( offset, 0, 0 ) ( bigChars, newOffset )

                newRow : Int
                newRow =
                    Pine_kernel.int_add [ row, newlineCount ]

                newCol : Int
                newCol =
                    if Pine_kernel.equal [ newlineCount, 0 ] then
                        Pine_kernel.int_add [ col, colShift ]

                    else
                        Pine_kernel.int_add [ 1, colShift ]
            in
            ( newOffset, newRow, newCol )


        indexOf : List Char -> List Char -> Int -> Int
        indexOf smallChars bigChars offset =
            let
                expectedLength : Int
                expectedLength =
                    Pine_kernel.length smallChars

                sliceFromSourceChars : List Char
                sliceFromSourceChars =
                    Pine_kernel.take
                        [ expectedLength
                        , Pine_kernel.skip [ offset, bigChars ]
                        ]
            in
            if Pine_kernel.equal [ Pine_kernel.length sliceFromSourceChars, expectedLength ] then
                if Pine_kernel.equal [ sliceFromSourceChars, smallChars ] then
                    offset

                else
                    indexOf smallChars bigChars (Pine_kernel.int_add [ offset, 1 ])

            else
                -1

     * */

    static PrecompiledResult.FinalValue? ElmKernelParser_findSubstring(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        if (PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0, 1, 0]) is not PineValue.ListValue patternList)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1])).IsOkOrNullable() is not { } offset)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2])).IsOkOrNullable() is not { } row)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3])).IsOkOrNullable() is not { } col)
        {
            return null;
        }

        if (PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]) is not PineValue.ListValue bigCharsList)
        {
            return null;
        }

        var newOffset = (int)offset;

        for (; newOffset < bigCharsList.Items.Length; ++newOffset)
        {
            if (bigCharsList.Items.Length <= newOffset + patternList.Items.Length)
            {
                newOffset = -1;
                break;
            }

            var found = true;

            for (var j = 0; j < patternList.Items.Length; ++j)
            {
                if (patternList.Items.Span[j] != bigCharsList.Items.Span[newOffset + j])
                {
                    found = false;
                    break;
                }
            }

            if (found)
            {
                break;
            }
        }

        var consumedLength = newOffset - (int)offset;

        var newRow = (int)row;
        var newCol = (int)col;

        for (var i = (int)offset; i < offset + consumedLength; ++i)
        {
            if (bigCharsList.Items.Span[i] == Character_ASCII_Newline_Value)
            {
                ++newRow;
                newCol = 1;
                continue;
            }

            ++newCol;
        }

        var finalValue =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(newOffset),
                IntegerEncoding.EncodeSignedInteger(newRow),
                IntegerEncoding.EncodeSignedInteger(newCol)
                ]);

        return
            new PrecompiledResult.FinalValue(
                finalValue,
                StackFrameCount: 0);
    }

    static PrecompiledResult.FinalValue? ElmKernelParser_chompWhileHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
         * chompWhileHelp : (Char -> Bool) -> ( Int, Int, Int ) -> List Char -> ( Int, Int, Int )
         * chompWhileHelp isGood ( offset, row, col ) srcChars =
         * 
         * */

        var isGoodFunctionValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var isMatchFunctionValueJson =
            System.Text.Json.JsonSerializer.Serialize(
                isGoodFunctionValue,
                EncodePineExpressionAsJson.BuildJsonSerializerOptions());

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(isGoodFunctionValue, parseCache).IsOkOrNull()
            is not { } isMatchFunctionRecord)
        {
            return null;
        }

        return null;
    }

    static PrecompiledResult.FinalValue? ElmKernelParser_chompWhileHelp_single_char_space_32(
        PineValue environment,
        PineVMParseCache parseCache) =>
        ElmKernelParser_chompWhileHelp_single_char(
            environment,
            PineValue.Blob([32]));

    static PrecompiledResult.FinalValue? ElmKernelParser_chompWhileHelp_single_char_minus_45(
        PineValue environment,
        PineVMParseCache parseCache) =>
        ElmKernelParser_chompWhileHelp_single_char(
            environment,
            PineValue.Blob([45]));

    static PrecompiledResult.FinalValue? ElmKernelParser_chompWhileHelp_not_carriage_return_or_newline(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var carriageReturnChar =
            PineValue.Blob([13]);

        var newlineChar =
            PineValue.Blob([10]);

        return
            ElmKernelParser_chompWhileHelp(
                environment,
                charValuePredicate:
                c => !(c == carriageReturnChar || c == newlineChar));
    }

    static PrecompiledResult.FinalValue? ElmKernelParser_chompWhileHelp_not_elm_multiline_comment_open_or_close(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var openChar =
            PineValue.Blob([(byte)'{']);

        var closeChar =
            PineValue.Blob([(byte)'-']);

        return
            ElmKernelParser_chompWhileHelp(
                environment,
                charValuePredicate:
                c => !(c == openChar || c == closeChar));
    }

    static PrecompiledResult.FinalValue? ElmKernelParser_chompWhileHelp_is_alpha_num_or_underscore(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        static bool charValuePredicate(PineValue charValue)
        {
            if (charValue is not PineValue.BlobValue blobValue)
            {
                return false;
            }

            if (blobValue.Bytes.Length is not 1)
            {
                return false;
            }

            var byteValue = blobValue.Bytes.Span[0];

            return
                (byteValue >= 48 && byteValue <= 57) ||
                (byteValue >= 65 && byteValue <= 90) ||
                (byteValue >= 97 && byteValue <= 122) ||
                byteValue == '_';
        }

        return
            ElmKernelParser_chompWhileHelp(
                environment,
                charValuePredicate:
                charValuePredicate);
    }

    static PrecompiledResult.FinalValue? ElmKernelParser_chompWhileHelp_single_char(
        PineValue environment,
        PineValue charValue)
    {
        return
            ElmKernelParser_chompWhileHelp(
                environment,
                charValuePredicate: c => c == charValue);
    }

    static PrecompiledResult.FinalValue? ElmKernelParser_chompWhileHelp(
        PineValue environment,
        Func<PineValue, bool> charValuePredicate)
    {
        /*
         * chompWhileHelp : (Char -> Bool) -> ( Int, Int, Int ) -> List Char -> ( Int, Int, Int )
         * chompWhileHelp isGood ( offset, row, col ) srcChars =
         * 
         * */

        var offsetValue =
        PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1, 0]);

        var rowValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1, 1]);

        var colValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1, 2]);

        var srcCharsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(offsetValue).IsOkOrNullable() is not { } startOffset)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(rowValue).IsOkOrNullable() is not { } row)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(colValue).IsOkOrNullable() is not { } col)
        {
            return null;
        }

        if (srcCharsValue is not PineValue.ListValue srcCharsList)
        {
            return null;
        }

        if (startOffset < 0)
        {
            return null;
        }

        /*
         * In contrast to countOffsetsInString, chompWhileHelp uses 1-based indexing for row and col.
         * */

        var offset = (int)startOffset;

        while (true)
        {
            if (srcCharsList.Items.Length <= offset)
            {
                break;
            }

            var currentChar = srcCharsList.Items.Span[offset];

            if (!charValuePredicate(currentChar))
            {
                break;
            }

            ++offset;

            if (currentChar == Character_ASCII_Newline_Value)
            {
                ++row;
                col = 1;
            }
            else
            {
                ++col;
            }
        }

        var finalValue =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(offset),
                IntegerEncoding.EncodeSignedInteger(row),
                IntegerEncoding.EncodeSignedInteger(col)
                ]);

        return
            new PrecompiledResult.FinalValue(
                finalValue,
                StackFrameCount: 0);
    }

    static Func<PrecompiledResult>? ListMapHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentMapFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentRemainingItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var argumentMappedItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (argumentMappedItems is not PineValue.ListValue mappedList)
        {
            return null;
        }

        var mappedReversed =
            KernelFunction.reverse(argumentMappedItems);

        if (argumentRemainingItems is not PineValue.ListValue remainingItemsListValue)
        {
            return null;
        }

        if (remainingItemsListValue.Items.Length < 1)
        {
            return () => new PrecompiledResult.FinalValue(mappedReversed, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.ParameterCount -
            functionRecordOk.Value.ArgumentsAlreadyCollected.Length;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.EnvFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsItems = new PineValue[functionRecordOk.Value.ArgumentsAlreadyCollected.Length + 1];

            functionRecordOk.Value.ArgumentsAlreadyCollected.CopyTo(argumentsItems);

            argumentsItems[^1] = itemValue;

            var argumentsList = PineValue.List(argumentsItems);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemsResults = new PineValue[remainingItemsListValue.Items.Length];
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            itemsResults[itemIndex] = itemResultValue;

            ++itemIndex;

            if (itemIndex < remainingItemsListValue.Items.Length)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(remainingItemsListValue.Items.Span[itemIndex]),
                        Callback: step);
            }

            var resultValue =
                KernelFunctionSpecialized.concat(mappedReversed, PineValue.List(itemsResults));

            return new PineVM.ApplyStepwise.StepResult.Complete(resultValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(remainingItemsListValue.Items.Span[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ListConcatMap(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentMapFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (argumentItems is not PineValue.ListValue itemsListValue)
        {
            return null;
        }

        if (itemsListValue.Items.Length < 1)
        {
            return () => new PrecompiledResult.FinalValue(PineValue.EmptyList, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.ParameterCount -
            functionRecordOk.Value.ArgumentsAlreadyCollected.Length;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.EnvFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsItems = new PineValue[functionRecordOk.Value.ArgumentsAlreadyCollected.Length + 1];

            functionRecordOk.Value.ArgumentsAlreadyCollected.CopyTo(argumentsItems);

            argumentsItems[^1] = itemValue;

            return
                PineValue.List(
                    [
                    environmentFunctionsEntry,
                    PineValue.List(argumentsItems)
                    ]);
        }

        var itemsResults = new PineValue[itemsListValue.Items.Length];
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            itemsResults[itemIndex] = itemResultValue;

            ++itemIndex;

            if (itemIndex < itemsListValue.Items.Length)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Items.Span[itemIndex]),
                        Callback: step);
            }

            var concatResult = KernelFunction.concat(PineValue.List(itemsResults));

            return new PineVM.ApplyStepwise.StepResult.Complete(concatResult);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Items.Span[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ListFoldl(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentMapFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentAggregate =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var argumentItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (argumentItems is not PineValue.ListValue itemsListValue)
        {
            return null;
        }

        if (itemsListValue.Items.Length < 1)
        {
            return () => new PrecompiledResult.FinalValue(argumentAggregate, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.ParameterCount -
            functionRecordOk.Value.ArgumentsAlreadyCollected.Length;

        if (functionRecordRemainingParamCount is not 2)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.EnvFunctions);

        PineValue environmentForItem(PineValue aggregate, PineValue itemValue)
        {
            var argumentsItems = new PineValue[functionRecordOk.Value.ArgumentsAlreadyCollected.Length + 2];

            functionRecordOk.Value.ArgumentsAlreadyCollected.CopyTo(argumentsItems);

            argumentsItems[^2] = itemValue;

            argumentsItems[^1] = aggregate;

            var argumentsList = PineValue.List(argumentsItems);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var mutatedAggregate = argumentAggregate;
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            mutatedAggregate = itemResultValue;

            ++itemIndex;

            if (itemIndex < itemsListValue.Items.Length)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(mutatedAggregate, itemsListValue.Items.Span[itemIndex]),
                        Callback: step);
            }

            if (false)
            {
                var resultFromVM =
                    new PineVM(
                        disablePrecompiled: true,
                        disableReductionInCompilation: true)
                    .EvaluateExpression(
                        expression: popularExpressionDictionary["List.foldl"],
                        environment: environment)
                    .Extract(err => throw new Exception(err));

                if (resultFromVM != mutatedAggregate)
                {
                    throw new Exception("foldl result mismatch");
                }
            }

            return new PineVM.ApplyStepwise.StepResult.Complete(mutatedAggregate);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(mutatedAggregate, itemsListValue.Items.Span[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ListFilterHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentPredicateFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentListValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var argumentAccumulated =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (argumentListValue is not PineValue.ListValue itemsList)
        {
            return null;
        }

        var accumulatedReversed =
            KernelFunction.reverse(argumentAccumulated);

        if (itemsList.Items.Length < 1)
        {
            return () => new PrecompiledResult.FinalValue(accumulatedReversed, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentPredicateFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.ParameterCount -
            functionRecordOk.Value.ArgumentsAlreadyCollected.Length;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.EnvFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsItems = new PineValue[functionRecordOk.Value.ArgumentsAlreadyCollected.Length + 1];

            functionRecordOk.Value.ArgumentsAlreadyCollected.CopyTo(argumentsItems);

            argumentsItems[^1] = itemValue;

            var argumentsList = PineValue.List(argumentsItems);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var includedItems = new PineValue[itemsList.Items.Length];
        var itemIndex = 0;
        var includedItemCount = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            if (itemResultValue == PineKernelValues.TrueValue)
            {
                includedItems[includedItemCount] = itemsList.Items.Span[itemIndex];

                ++includedItemCount;
            }

            ++itemIndex;

            if (itemIndex < itemsList.Items.Length)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsList.Items.Span[itemIndex]),
                        Callback: step);
            }

            var resultValue =
                KernelFunctionSpecialized.concat(
                    accumulatedReversed,
                    PineValue.List(includedItems[..includedItemCount]));

            return new PineVM.ApplyStepwise.StepResult.Complete(resultValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsList.Items.Span[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ListFilterMapHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        filterMapHelp : (a -> Maybe b) -> List a -> List b -> List b
        filterMapHelp f xs acc =
            case xs of
                [] ->
                    Pine_kernel.reverse acc

                x :: remaining ->
                    case f x of
                        Just value ->
                            filterMapHelp f remaining (Pine_kernel.concat [ [ value ], acc ])

                        Nothing ->
                            filterMapHelp f remaining acc

         * */

        var argumentMapItem =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentListValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var argumentAccumulated =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (argumentListValue is not PineValue.ListValue itemsList)
        {
            return null;
        }

        var accumulatedReversed =
            KernelFunction.reverse(argumentAccumulated);

        if (itemsList.Items.Length < 1)
        {
            return () => new PrecompiledResult.FinalValue(accumulatedReversed, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapItem, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.ParameterCount -
            functionRecordOk.Value.ArgumentsAlreadyCollected.Length;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.EnvFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsItems = new PineValue[functionRecordOk.Value.ArgumentsAlreadyCollected.Length + 1];

            functionRecordOk.Value.ArgumentsAlreadyCollected.CopyTo(argumentsItems);

            argumentsItems[^1] = itemValue;

            var argumentsList = PineValue.List(argumentsItems);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var includedItems = new PineValue[itemsList.Items.Length];

        var itemIndex = 0;

        var includedItemCount = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            var itemResultValueTag =
                PineVM.ValueFromPathInValueOrEmptyList(itemResultValue, [0]);

            if (itemResultValueTag == Tag_Just_Name_Value)
            {
                includedItems[includedItemCount] =
                    PineVM.ValueFromPathInValueOrEmptyList(itemResultValue, [1, 0]);

                ++includedItemCount;
            }
            else
            {
                if (itemResultValueTag != Tag_Nothing_Name_Value)
                {
                    throw new ParseExpressionException("Error in case-of block: No matching branch.");
                }
            }

            ++itemIndex;

            if (itemIndex < itemsList.Items.Length)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsList.Items.Span[itemIndex]),
                        Callback: step);
            }

            var resultValue =
                KernelFunctionSpecialized.concat(
                    accumulatedReversed,
                    PineValue.List(includedItems[..includedItemCount]));

            return new PineVM.ApplyStepwise.StepResult.Complete(resultValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsList.Items.Span[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ListAll(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentMapFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (argumentItems is not PineValue.ListValue itemsListValue)
        {
            return null;
        }

        if (itemsListValue.Items.Length < 1)
        {
            return () => new PrecompiledResult.FinalValue(PineKernelValues.TrueValue, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.ParameterCount -
            functionRecordOk.Value.ArgumentsAlreadyCollected.Length;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.EnvFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsItems = new PineValue[functionRecordOk.Value.ArgumentsAlreadyCollected.Length + 1];

            functionRecordOk.Value.ArgumentsAlreadyCollected.CopyTo(argumentsItems);

            argumentsItems[^1] = itemValue;

            var argumentsList = PineValue.List(argumentsItems);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            if (itemResultValue != PineKernelValues.TrueValue)
            {
                return new PineVM.ApplyStepwise.StepResult.Complete(PineKernelValues.FalseValue);
            }

            ++itemIndex;

            if (itemIndex < itemsListValue.Items.Length)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Items.Span[itemIndex]),
                        Callback: step);
            }

            return
                new PineVM.ApplyStepwise.StepResult.Complete(PineKernelValues.TrueValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Items.Span[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ListAny(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentMapFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (argumentItems is not PineValue.ListValue itemsListValue)
        {
            return null;
        }

        if (itemsListValue.Items.Length < 1)
        {
            return () => new PrecompiledResult.FinalValue(PineKernelValues.FalseValue, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.ParameterCount -
            functionRecordOk.Value.ArgumentsAlreadyCollected.Length;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.EnvFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsItems = new PineValue[functionRecordOk.Value.ArgumentsAlreadyCollected.Length + 1];

            functionRecordOk.Value.ArgumentsAlreadyCollected.CopyTo(argumentsItems);

            argumentsItems[^1] = itemValue;

            var argumentsList = PineValue.List(argumentsItems);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            if (itemResultValue == PineKernelValues.TrueValue)
            {
                return new PineVM.ApplyStepwise.StepResult.Complete(PineKernelValues.TrueValue);
            }

            ++itemIndex;

            if (itemIndex < itemsListValue.Items.Length)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Items.Span[itemIndex]),
                        Callback: step);
            }

            return
                new PineVM.ApplyStepwise.StepResult.Complete(PineKernelValues.FalseValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.InnerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Items.Span[itemIndex]),
                        Callback: step)));
    }

    private static PrecompiledResult.FinalValue? ListIntersperseHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        intersperseHelp : List a -> Int -> a -> List a -> List a
        intersperseHelp acc offset sep xs =
            case Pine_kernel.take [ 1, Pine_kernel.skip [ offset, xs ] ] of
                [ x ] ->
                    intersperseHelp
                        (Pine_kernel.concat [ acc, [ sep, x ] ])
                        (offset + 1)
                        sep
                        xs

                _ ->
                    acc
         * */

        var accValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var sepValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        var sourceListValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        if (sourceListValue is not PineValue.ListValue sourceList)
        {
            return null;
        }

        if (accValue is not PineValue.ListValue accList)
        {
            return null;
        }

        if (sourceList.Items.Length is 0)
        {
            return
                new PrecompiledResult.FinalValue(
                    accValue,
                    StackFrameCount: 0);
        }

        if (KernelFunction.SignedIntegerFromValueRelaxed(offsetValue) is not { } offset)
        {
            return null;
        }

        var offsetIndex = (int)offset;

        var aggregateLength =
            accList.Items.Length
            +
            (sourceList.Items.Length - offsetIndex) * 2;

        var aggregate = new PineValue[aggregateLength];

        var aggregateIndex = 0;

        for (var i = 0; i < offsetIndex; ++i)
        {
            aggregate[aggregateIndex] = accList.Items.Span[i];

            ++aggregateIndex;
        }

        for (var i = offsetIndex; i < sourceList.Items.Length; ++i)
        {
            aggregate[aggregateIndex] = sepValue;
            ++aggregateIndex;

            aggregate[aggregateIndex] = sourceList.Items.Span[i];
            ++aggregateIndex;
        }

        return
            new PrecompiledResult.FinalValue(
                PineValue.List(aggregate),
                StackFrameCount: 0);
    }

    static Func<PrecompiledResult>? StringSlice(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        slice : Int -> Int -> String -> String
        slice start end (String charsBlob) =
            if Pine_kernel.int_is_sorted_asc [ 0, start, end ] then
                let
                    sliceLength : Int
                    sliceLength =
                        Pine_kernel.int_add [ end, Pine_kernel.int_mul [ -1, start ] ]
                in
                String
                    (Pine_kernel.take
                        [ Pine_kernel.int_mul [ sliceLength, 4 ]
                        , Pine_kernel.skip
                            [ Pine_kernel.int_mul [ start, 4 ]
                            , charsBlob
                            ]
                        ]
                    )

            else
                let
                    absoluteIndex relativeIndex =
                        {-
                           Instead of using integer comparison together with the literal 0,
                           check the first byte if the sign is negative.
                        -}
                        if
                            Pine_kernel.equal
                                [ Pine_kernel.take [ 1, relativeIndex ]
                                , Pine_kernel.take [ 1, -1 ]
                                ]
                        then
                            Pine_kernel.int_add [ relativeIndex, Pine_kernel.length charsBlob ]

                        else
                            relativeIndex

                    absoluteStart : Int
                    absoluteStart =
                        absoluteIndex
                            (Pine_kernel.int_mul [ start, 4 ])

                    sliceLength : Int
                    sliceLength =
                        Pine_kernel.int_add
                            [ absoluteIndex (Pine_kernel.int_mul [ end, 4 ])
                            , Pine_kernel.int_mul [ -1, absoluteStart ]
                            ]
                in
                String
                    (Pine_kernel.take
                        [ sliceLength
                        , Pine_kernel.skip [ absoluteStart, charsBlob ]
                        ]
                    )

         * */

        var startValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var endValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var charsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2, 1, 0]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(startValue).IsOkOrNullable() is not { } start)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(endValue).IsOkOrNullable() is not { } end)
        {
            return null;
        }

        if (charsValue is not PineValue.BlobValue charsBlob)
        {
            return null;
        }

        int absoluteIndex(int relativeIndex)
        {
            if (relativeIndex < 0)
            {
                return relativeIndex + charsBlob.Bytes.Length;
            }

            return relativeIndex;
        }

        var absoluteStart = absoluteIndex((int)start * 4);

        var sliceLength = absoluteIndex((int)end * 4) - absoluteStart;

        var sliceCharsBytes =
            charsBlob.Bytes.Length <= absoluteStart
            ?
            PineValue.EmptyBlob.Bytes
            :
            charsBlob.Bytes.Slice(
                start: absoluteStart,
                length: Math.Max(0, sliceLength));

        var slicedStringValue =
            PineValue.List(
                [
                Tag_String_Value,
                PineValue.List([PineValue.Blob(sliceCharsBytes)])
                ]);

        var finalValue =
            new PrecompiledResult.FinalValue(
                slicedStringValue,
                StackFrameCount: 0);

        return () => finalValue;
    }

    static Func<PrecompiledResult>? StringLinesHelper(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        linesHelper : Int -> List String -> Int -> Int -> List String
        linesHelper currentLineStart currentLines offset charsBytes =
            let
                nextChar =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offset, charsBytes ]
                        ]

                nextTwoChars =
                    Pine_kernel.take
                        [ 8
                        , Pine_kernel.skip [ offset, charsBytes ]
                        ]
            in
            if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                let
                    currentLineLength =
                        Pine_kernel.int_add [ offset, -currentLineStart ]
                in
                Pine_kernel.concat
                    [ currentLines
                    , [ String (Pine_kernel.skip [ currentLineStart, charsBytes ]) ]
                    ]

            else if Pine_kernel.equal [ nextTwoChars, Pine_kernel.concat [ '\u{000D}', '\n' ] ] then
                let
                    currentLineLength =
                        Pine_kernel.int_add [ offset, -currentLineStart ]

                    currentLineChars : Int
                    currentLineChars =
                        Pine_kernel.take
                            [ currentLineLength
                            , Pine_kernel.skip [ currentLineStart, charsBytes ]
                            ]
                in
                linesHelper
                    (Pine_kernel.int_add [ offset, 8 ])
                    (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
                    (Pine_kernel.int_add [ offset, 8 ])
                    charsBytes

            else if Pine_kernel.equal [ nextChar, '\n' ] then
                let
                    currentLineLength =
                        Pine_kernel.int_add [ offset, -currentLineStart ]

                    currentLineChars : List Char
                    currentLineChars =
                        Pine_kernel.take
                            [ currentLineLength
                            , Pine_kernel.skip [ currentLineStart, charsBytes ]
                            ]
                in
                linesHelper
                    (Pine_kernel.int_add [ offset, 4 ])
                    (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
                    (Pine_kernel.int_add [ offset, 4 ])
                    charsBytes

            else if Pine_kernel.equal [ nextChar, '\u{000D}' ] then
                let
                    currentLineLength =
                        Pine_kernel.int_add [ offset, -currentLineStart ]

                    currentLineChars : List Char
                    currentLineChars =
                        Pine_kernel.take
                            [ currentLineLength
                            , Pine_kernel.skip [ currentLineStart, charsBytes ]
                            ]
                in
                linesHelper
                    (Pine_kernel.int_add [ offset, 4 ])
                    (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
                    (Pine_kernel.int_add [ offset, 4 ])
                    charsBytes

            else
                linesHelper
                    currentLineStart
                    currentLines
                    (Pine_kernel.int_add [ offset, 4 ])
                    charsBytes

         * */

        var currentLineStartValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var currentLinesValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        var charsBytesValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(currentLineStartValue).IsOkOrNullable() is not { } currentLineStart)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(offsetValue).IsOkOrNullable() is not { } offset)
        {
            return null;
        }

        if (charsBytesValue is not PineValue.BlobValue charsBlob)
        {
            return null;
        }

        var offsetInt = (int)offset;

        var currentLineStartInt = (int)currentLineStart;

        var linesValues = new List<PineValue>();

        while (true)
        {
            var remainingBytesCount =
                charsBlob.Bytes.Length - offsetInt;

            if (remainingBytesCount < 1)
            {
                break;
            }

            if (remainingBytesCount < 4)
            {
                continue;
            }

            var nextChar = BinaryPrimitives.ReadInt32BigEndian(charsBlob.Bytes.Span[offsetInt..]);

            if (8 <= remainingBytesCount)
            {
                var nextNextChar =
                    BinaryPrimitives.ReadInt32BigEndian(charsBlob.Bytes.Span[(offsetInt + 4)..]);

                if (nextChar is 0x0d && nextNextChar is 0x0a)
                {
                    var currentLineLength =
                        offsetInt - currentLineStartInt;

                    var currentLineCharsBytes =
                        charsBlob.Bytes.Slice(
                            currentLineStartInt,
                            currentLineLength);

                    linesValues.Add(
                        PineValue.List(
                            [Tag_String_Value,
                            PineValue.List([PineValue.Blob(currentLineCharsBytes)])
                            ]));

                    offsetInt += 8;
                    currentLineStartInt = offsetInt;

                    continue;
                }
            }

            if (nextChar is 0x0a || nextChar is 0x0d)
            {
                var currentLineLength =
                    offsetInt - currentLineStartInt;

                var currentLineCharsBytes =
                    charsBlob.Bytes.Slice(
                        currentLineStartInt,
                        currentLineLength);

                linesValues.Add(
                    PineValue.List(
                        [Tag_String_Value,
                        PineValue.List([PineValue.Blob(currentLineCharsBytes)])
                        ]));

                offsetInt += 4;
                currentLineStartInt = offsetInt;

                continue;
            }

            offsetInt += 4;
        }

        var lastLineCharsBytes =
            charsBlob.Bytes[currentLineStartInt..];

        linesValues.Add(
            PineValue.List(
                [Tag_String_Value,
                    PineValue.List([PineValue.Blob(lastLineCharsBytes)])
                ]));

        var finalValue =
            KernelFunctionSpecialized.concat(
                currentLinesValue, PineValue.List([.. linesValues]));

        return
            () => new PrecompiledResult.FinalValue(finalValue, StackFrameCount: 0);
    }

    static Func<PrecompiledResult>? StringToListRecursive(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*

        toListRecursive : Int -> List Char -> Int -> List Char
        toListRecursive offset list blob =
            let
                nextChar =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offset, blob ]
                        ]
            in
            if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                list

            else
                toListRecursive
                    (Pine_kernel.int_add [ offset, 4 ])
                    (Pine_kernel.concat [ list, [ nextChar ] ])
                    blob
         * */

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var mappedCharsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var charsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(offsetValue).IsOkOrNullable() is not { } offset)
        {
            return null;
        }

        if (charsValue is not PineValue.BlobValue charsBlob)
        {
            return null;
        }

        var charsItems = new List<PineValue>(capacity: charsBlob.Bytes.Length / 3);

        var offsetInt = (int)offset;

        while (true)
        {
            var remainingBytesCount =
                charsBlob.Bytes.Length - offsetInt;

            if (remainingBytesCount < 1)
            {
                break;
            }

            var nextCharBytes =
                remainingBytesCount < 4
                ? charsBlob.Bytes.Slice(offsetInt, remainingBytesCount)
                : charsBlob.Bytes.Slice(offsetInt, 4);

            offsetInt += 4;

            charsItems.Add(PineValue.Blob(nextCharBytes));
        }

        var finalValue =
            KernelFunctionSpecialized.concat(
                mappedCharsValue, PineValue.List([.. charsItems]));

        return () => new PrecompiledResult.FinalValue(finalValue, StackFrameCount: 0);
    }

    static Func<PrecompiledResult>? String_splitHelperOnBlob(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        splitHelperOnBlob : Int -> List String -> Int -> Int -> Int -> List String
        splitHelperOnBlob offset collected lastStart sepBytes stringBytes =
            let
                sliceBytes : Int
                sliceBytes =
                    Pine_kernel.take
                        [ Pine_kernel.length sepBytes
                        , Pine_kernel.skip [ offset, stringBytes ]
                        ]
            in
            if Pine_kernel.equal [ sliceBytes, sepBytes ] then
                let
                    separatedSliceLength : Int
                    separatedSliceLength =
                        Pine_kernel.int_add
                            [ offset
                            , Pine_kernel.int_mul [ -1, lastStart ]
                            ]

                    separatedSlice : Int
                    separatedSlice =
                        Pine_kernel.take
                            [ separatedSliceLength
                            , Pine_kernel.skip [ lastStart, stringBytes ]
                            ]
                in
                splitHelperOnBlob
                    (Pine_kernel.int_add [ offset, Pine_kernel.length sepBytes ])
                    (Pine_kernel.concat [ collected, [ String separatedSlice ] ])
                    (Pine_kernel.int_add [ offset, Pine_kernel.length sepBytes ])
                    sepBytes
                    stringBytes

            else if Pine_kernel.equal [ Pine_kernel.length sliceBytes, 0 ] then
                let
                    separatedSlice : Int
                    separatedSlice =
                        Pine_kernel.skip [ lastStart, stringBytes ]
                in
                Pine_kernel.concat [ collected, [ String separatedSlice ] ]

            else
                splitHelperOnBlob
                    (Pine_kernel.int_add [ offset, 4 ])
                    collected
                    lastStart
                    sepBytes
                    stringBytes

         * */

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var collectedValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var lastStartValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        var sepBytesValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        var stringBytesValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]);

        if (KernelFunction.SignedIntegerFromValueRelaxed(offsetValue) is not { } offset)
        {
            return null;
        }

        if (collectedValue is not PineValue.ListValue collectedList)
        {
            return null;
        }

        if (sepBytesValue is not PineValue.BlobValue sepBytesBlob)
        {
            return null;
        }

        if (stringBytesValue is not PineValue.BlobValue stringBytesBlob)
        {
            return null;
        }

        if (KernelFunction.SignedIntegerFromValueRelaxed(lastStartValue) is not { } lastStart)
        {
            return null;
        }

        var offsetInt = (int)offset;

        var lastStartInt = (int)lastStart;

        if (offsetInt < 0)
            return null;

        if (lastStartInt < 0)
            return null;

        var collected = new List<PineValue>(collectedList.Items.ToArray());

        while (offsetInt + sepBytesBlob.Bytes.Length <= stringBytesBlob.Bytes.Length)
        {
            var stringSlice = stringBytesBlob.Bytes.Slice(start: offsetInt, length: sepBytesBlob.Bytes.Length);

            if (MemoryExtensions.SequenceEqual(stringSlice.Span, sepBytesBlob.Bytes.Span))
            {
                var separatedSliceLength = offsetInt - lastStartInt;

                var separatedSlice =
                    stringBytesBlob.Bytes.Slice(start: lastStartInt, length: separatedSliceLength);

                collected.Add(
                    PineValue.List(
                        [
                        Tag_String_Value,
                        PineValue.List([PineValue.Blob(separatedSlice)])
                        ]));

                offsetInt += sepBytesBlob.Bytes.Length;

                lastStartInt = offsetInt;
            }
            else
            {
                offsetInt += 4;
            }
        }

        var lastSlice = stringBytesBlob.Bytes[lastStartInt..];

        collected.Add(
            PineValue.List(
                [
                Tag_String_Value,
                PineValue.List([PineValue.Blob(lastSlice)])
                ]));

        var finalValue = PineValue.List([.. collected]);

        return () => new PrecompiledResult.FinalValue(finalValue, StackFrameCount: 0);
    }

    static Func<PrecompiledResult>? Elm_Kernel_Json_Decode_skipWhitespace(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        skipWhitespace : Int -> Int -> Int
        skipWhitespace strBytes offset =
            let
                nextChar =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offset, strBytes ]
                        ]
            in
            case nextChar of
                ' ' ->
                    skipWhitespace strBytes (offset + 4)

                '\t' ->
                    skipWhitespace strBytes (offset + 4)

                '\n' ->
                    skipWhitespace strBytes (offset + 4)

                '\u{000D}' ->
                    skipWhitespace strBytes (offset + 4)

                _ ->
                    offset

         * */

        var strBytesValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (strBytesValue is not PineValue.BlobValue strBytesBlob)
        {
            return null;
        }

        if (KernelFunction.SignedIntegerFromValueRelaxed(offsetValue) is not { } offset)
        {
            return null;
        }

        var offsetInt = (int)offset;

        while (true)
        {
            var remainingBytesCount =
                strBytesBlob.Bytes.Length - offsetInt;

            if (remainingBytesCount < 4)
            {
                break;
            }

            var nextChar = BinaryPrimitives.ReadInt32BigEndian(strBytesBlob.Bytes.Span[offsetInt..]);

            if (nextChar is 32 || nextChar is 9 || nextChar is 10 || nextChar is 13)
            {
                offsetInt += 4;
                continue;
            }

            break;
        }

        var finalValue =
            new PrecompiledResult.FinalValue(
                IntegerEncoding.EncodeSignedInteger(offsetInt),
                StackFrameCount: 0);

        return () => finalValue;
    }

    static Func<PrecompiledResult>? ParserFast_skipWhileWhitespaceHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        skipWhileWhitespaceHelp : Int -> Int -> Int -> List Char -> Int -> State
        skipWhileWhitespaceHelp offset row col src indent =
            case List.take 1 (List.drop offset src) of
                [ ' ' ] ->
                    skipWhileWhitespaceHelp (offset + 1) row (col + 1) src indent

                [ '\n' ] ->
                    skipWhileWhitespaceHelp (offset + 1) (row + 1) 1 src indent

                [ '\u{000D}' ] ->
                    skipWhileWhitespaceHelp (offset + 1) row (col + 1) src indent

                -- empty or non-whitespace
                _ ->
                    PState src offset indent row col

         * */


        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var rowValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var colValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        var srcCharsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        var indentValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(offsetValue).IsOkOrNullable() is not { } startOffset)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(rowValue).IsOkOrNullable() is not { } row)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(colValue).IsOkOrNullable() is not { } col)
        {
            return null;
        }

        if (srcCharsValue is not PineValue.ListValue srcCharsList)
        {
            return null;
        }

        var offset = (int)startOffset;

        while (true)
        {
            if (srcCharsList.Items.Length <= offset)
            {
                break;
            }

            var currentChar = srcCharsList.Items.Span[offset];

            if (currentChar is not PineValue.BlobValue currentCharBlob)
            {
                break;
            }

            if (currentCharBlob.Bytes.Length is not 4)
            {
                break;
            }

            if (currentCharBlob.Bytes.Span[0] is not 0 ||
                currentCharBlob.Bytes.Span[1] is not 0 ||
                currentCharBlob.Bytes.Span[2] is not 0)
            {
                break;
            }

            var byteValue = currentCharBlob.Bytes.Span[3];

            if (byteValue is 32)
            {
                ++offset;
                ++col;
                continue;
            }

            if (byteValue is 10)
            {
                ++offset;
                ++row;
                col = 1;
                continue;
            }

            if (byteValue is 13)
            {
                ++offset;
                ++col;
                continue;
            }

            break;
        }

        var result =
            new PrecompiledResult.FinalValue(
                PineValue.List(
                    [
                    Tag_PState_Name_Value,
                    PineValue.List(
                        [
                        srcCharsValue,
                        IntegerEncoding.EncodeSignedInteger(offset),
                        indentValue,
                        IntegerEncoding.EncodeSignedInteger(row),
                        IntegerEncoding.EncodeSignedInteger(col),
                        ])
                    ]),
                StackFrameCount: 0);

        return () => result;
    }


    static Func<PrecompiledResult>? ParserFast_skipWhileWithoutLinebreakHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        skipWhileWithoutLinebreakHelp : (Char -> Bool) -> Int -> Int -> Int -> List Char -> Int -> State
        skipWhileWithoutLinebreakHelp isGood offset row col src indent =
            case List.take 1 (List.drop offset src) of
                actualChar :: _ ->
                    if isGood actualChar then
                        skipWhileWithoutLinebreakHelp isGood (offset + 1) row (col + 1) src indent

                    else
                        -- no match
                        PState src offset indent row col

                [] ->
                    -- no match
                    PState src offset indent row col
         * */

        var isGoodFunctionValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var isMatchFunctionValueJson =
            System.Text.Json.JsonSerializer.Serialize(
                isGoodFunctionValue,
                EncodePineExpressionAsJson.BuildJsonSerializerOptions());

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(isGoodFunctionValue, parseCache).IsOkOrNull()
            is not { } isMatchFunctionRecord)
        {
            return null;
        }

        return null;
    }

    static Func<PrecompiledResult>? ParserFast_skipWhileWithoutLinebreakHelp_isAlphaNumOrUnderscore(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        isAlphaNumOrUnderscore : Char -> Bool
        isAlphaNumOrUnderscore c =
            Char.isAlphaNum c || c == '_'
         * */

        static bool charValuePredicate(PineValue charValue)
        {
            if (charValue is not PineValue.BlobValue blobValue)
            {
                return false;
            }

            if (blobValue.Bytes.Length is not 4)
            {
                return false;
            }

            if (blobValue.Bytes.Span[0] is not 0 ||
                blobValue.Bytes.Span[1] is not 0 ||
                blobValue.Bytes.Span[2] is not 0)
            {
                return false;
            }

            var byteValue = blobValue.Bytes.Span[3];

            return
                (byteValue >= 48 && byteValue <= 57) ||
                (byteValue >= 65 && byteValue <= 90) ||
                (byteValue >= 97 && byteValue <= 122) ||
                byteValue == '_';
        }

        return
            ParserFast_skipWhileWithoutLinebreakHelp(
                environment,
                parseCache,
                charValuePredicate);
    }

    static Func<PrecompiledResult>? ParserFast_skipWhileWithoutLinebreakHelp_is_not_ASCII_quote_or_backslash(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        static bool charValuePredicate(PineValue charValue)
        {
            if (charValue is not PineValue.BlobValue blobValue)
            {
                return false;
            }

            if (blobValue.Bytes.Length is not 4)
            {
                return false;
            }

            if (blobValue.Bytes.Span[0] is not 0 ||
                blobValue.Bytes.Span[1] is not 0 ||
                blobValue.Bytes.Span[2] is not 0)
            {
                return true;
            }

            var byteValue = blobValue.Bytes.Span[3];

            return
                byteValue != '"' && byteValue != '\\';
        }

        return
            ParserFast_skipWhileWithoutLinebreakHelp(
                environment,
                parseCache,
                charValuePredicate);
    }

    static Func<PrecompiledResult>? ParserFast_skipWhileWithoutLinebreakHelp(
        PineValue environment,
        PineVMParseCache parseCache,
        Func<PineValue, bool> charValuePredicate)
    {
        /*
        skipWhileWithoutLinebreakHelp : (Char -> Bool) -> Int -> Int -> Int -> List Char -> Int -> State
        skipWhileWithoutLinebreakHelp isGood offset row col src indent =
            case List.take 1 (List.drop offset src) of
                actualChar :: _ ->
                    if isGood actualChar then
                        skipWhileWithoutLinebreakHelp isGood (offset + 1) row (col + 1) src indent

                    else
                        -- no match
                        PState src offset indent row col

                [] ->
                    -- no match
                    PState src offset indent row col
         * */

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var rowValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        var colValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        var srcCharsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]);

        var indentValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 5]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(offsetValue).IsOkOrNullable() is not { } startOffset)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(colValue).IsOkOrNullable() is not { } col)
        {
            return null;
        }

        if (srcCharsValue is not PineValue.ListValue srcCharsList)
        {
            return null;
        }

        var offset = (int)startOffset;

        while (true)
        {
            if (srcCharsList.Items.Length <= offset)
            {
                break;
            }

            var currentChar = srcCharsList.Items.Span[offset];

            if (charValuePredicate(currentChar))
            {
                ++offset;
                ++col;
                continue;
            }

            break;
        }

        var result =
            new PrecompiledResult.FinalValue(
                PineValue.List(
                    [
                    Tag_PState_Name_Value,
                    PineValue.List(
                        [
                        srcCharsValue,
                        IntegerEncoding.EncodeSignedInteger(offset),
                        indentValue,
                        rowValue,
                        IntegerEncoding.EncodeSignedInteger(col),
                        ])
                    ]),
                StackFrameCount: 0);

        return
            () => result;
    }

    static PrecompiledResult.FinalValue? ParserFast_skipWhileHelp_not_elm_multiline_comment_open_or_close(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var openChar =
            PineValue.Blob([0, 0, 0, (byte)'{']);

        var closeChar =
            PineValue.Blob([0, 0, 0, (byte)'-']);

        return
            ParserFast_skipWhileHelp(
                environment,
                charValuePredicate:
                c => !(c == openChar || c == closeChar));
    }

    static PrecompiledResult.FinalValue? ParserFast_skipWhileHelp_not_ASCII_quote_or_backslash(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        static bool charValuePredicate(PineValue charValue)
        {
            if (charValue is not PineValue.BlobValue blobValue)
            {
                return false;
            }

            if (blobValue.Bytes.Length is not 4)
            {
                return false;
            }

            if (blobValue.Bytes.Span[0] is not 0 ||
                blobValue.Bytes.Span[1] is not 0 ||
                blobValue.Bytes.Span[2] is not 0)
            {
                return true;
            }

            var byteValue = blobValue.Bytes.Span[3];

            return
                byteValue != '"' && byteValue != '\\';
        }

        return
            ParserFast_skipWhileHelp(
                environment,
                charValuePredicate: charValuePredicate);
    }

    static PrecompiledResult.FinalValue? ParserFast_skipWhileHelp(
        PineValue environment,
        Func<PineValue, bool> charValuePredicate)
    {
        /*
        skipWhileHelp : (Char -> Bool) -> Int -> Int -> Int -> List Char -> Int -> State
        skipWhileHelp isGood offset row col src indent =
            case List.take 1 (List.drop offset src) of
                actualChar :: _ ->
                    if isGood actualChar then
                        case actualChar of
                            '\n' ->
                                skipWhileHelp isGood (offset + 1) (row + 1) 1 src indent

                            _ ->
                                skipWhileHelp isGood (offset + 1) row (col + 1) src indent

                    else
                        -- no match
                        PState src offset indent row col

                [] ->
                    -- no match
                    PState src offset indent row col
         * 
         * */

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var rowValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        var colValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        var srcCharsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]);

        var indentValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 5]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(offsetValue).IsOkOrNullable() is not { } startOffset)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(rowValue).IsOkOrNullable() is not { } row)
        {
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(colValue).IsOkOrNullable() is not { } col)
        {
            return null;
        }

        if (srcCharsValue is not PineValue.ListValue srcCharsList)
        {
            return null;
        }

        if (startOffset < 0)
        {
            return null;
        }

        var offset = (int)startOffset;

        while (true)
        {
            if (srcCharsList.Items.Length <= offset)
            {
                break;
            }

            var currentChar = srcCharsList.Items.Span[offset];

            if (!charValuePredicate(currentChar))
            {
                break;
            }

            ++offset;

            if (currentChar == Character_ASCII_Newline_Value)
            {
                ++row;
                col = 1;
            }
            else
            {
                ++col;
            }
        }

        var finalValue =
            PineValue.List(
                [
                Tag_PState_Name_Value,
                PineValue.List(
                    [
                    srcCharsValue,
                    IntegerEncoding.EncodeSignedInteger(offset),
                    indentValue,
                    IntegerEncoding.EncodeSignedInteger(row),
                    IntegerEncoding.EncodeSignedInteger(col),
                    ])
                ]);

        return
            new PrecompiledResult.FinalValue(
                finalValue,
                StackFrameCount: 0);
    }

    static Func<PrecompiledResult>? ParserFast_skipWhileHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        skipWhileHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> State
        skipWhileHelp isGood offset row col src indent =
            let
                actualChar : String
                actualChar =
                    String.slice offset (offset + 1) src
            in
            if String.any isGood actualChar then
                case actualChar of
                    "\n" ->
                        skipWhileHelp isGood (offset + 1) (row + 1) 1 src indent

                    _ ->
                        skipWhileHelp isGood (offset + 1) row (col + 1) src indent

            else
                -- no match
                PState src offset indent row col
         * */

        var isGoodFunctionValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var isMatchFunctionValueJson =
            System.Text.Json.JsonSerializer.Serialize(
                isGoodFunctionValue,
                EncodePineExpressionAsJson.BuildJsonSerializerOptions());

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(isGoodFunctionValue, parseCache).IsOkOrNull()
            is not { } isMatchFunctionRecord)
        {
            return null;
        }

        return null;
    }

    static Func<PrecompiledResult>? ElmCompiledAdaptivePartialApplication(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        if (environment is not PineValue.ListValue envList)
            return null;

        if (envList.Items.Length < 2)
            return null;

        if (envList.Items.Span[1] is not PineValue.ListValue taggedFunctionRecordList)
            return null;

        if (envList.Items.Span[2] is not PineValue.ListValue newArgumentsList)
            return null;

        if (newArgumentsList.Items.Length is 0)
        {
            return () => new PrecompiledResult.FinalValue(taggedFunctionRecordList, StackFrameCount: 0);
        }

        if (taggedFunctionRecordList.Items.Length < 2)
            return null;

        /*
         * We already check this condition via the environment value class.
         * 
        if (taggedFunctionRecordList.Elements[0] != ElmCompilerFunctionTagValue)
            return null;
        */

        if (taggedFunctionRecordList.Items.Span[1] is not PineValue.ListValue functionRecord)
            return null;

        if (functionRecord.Items.Length < 4)
            return null;

        if (functionRecord.Items.Span[3] is not PineValue.ListValue argsCollectedPreviouslyList)
            return null;

        if (functionRecord.Items.Span[1] is not PineValue.BlobValue paramCountBlob)
            return null;

        if (paramCountBlob.Bytes.Length is not 2)
            return null;

        if (paramCountBlob.Bytes.Span[0] is not 4)
            return null;

        var paramCount = paramCountBlob.Bytes.Span[1];

        var envFunctions = functionRecord.Items.Span[2];

        if (paramCount != argsCollectedPreviouslyList.Items.Length + newArgumentsList.Items.Length)
            return null;

        PineValue? combinedArgumentsValue = null;

        if (argsCollectedPreviouslyList.Items.Length is 0)
        {
            combinedArgumentsValue = newArgumentsList;
        }
        else
        {
            var combinedArgsArray =
                new PineValue[argsCollectedPreviouslyList.Items.Length + newArgumentsList.Items.Length];

            for (var i = 0; i < argsCollectedPreviouslyList.Items.Length; ++i)
            {
                combinedArgsArray[i] = argsCollectedPreviouslyList.Items.Span[i];
            }

            for (var i = 0; i < newArgumentsList.Items.Length; ++i)
            {
                combinedArgsArray[i + argsCollectedPreviouslyList.Items.Length] = newArgumentsList.Items.Span[i];
            }

            combinedArgumentsValue = PineValue.List(combinedArgsArray);
        }

        var newEnvironment =
            PineValue.List(
                [
                envFunctions,
                combinedArgumentsValue
                ]);

        return
            () => new PrecompiledResult.ContinueParseAndEval(
                EnvironmentValue: newEnvironment,
                ExpressionValue: functionRecord.Items.Span[0]);
    }

    private static Func<PrecompiledResult>? ElmKernelJsonDecode_parseJsonStringSimpleChars(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        parseJsonStringSimpleChars : Int -> Int -> Int
        parseJsonStringSimpleChars sourceBytes offset =
            let
                nextChar =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip [ offset, sourceBytes ]
                        ]
            in
            if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                -- We ran out of characters before finding a closing quote or escape
                offset

            else if Pine_kernel.equal [ nextChar, '"' ] then
                -- Found a quote or backslash escape, stop here
                offset

            else if Pine_kernel.equal [ nextChar, '\\' ] then
                -- Found a backslash escape, stop here
                offset

            else
                -- Keep going to the next character
                parseJsonStringSimpleChars
                    sourceBytes
                    (Pine_kernel.int_add [ offset, 4 ])

         * */

        var sourceBytesArg =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var offsetArg =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (sourceBytesArg is not PineValue.BlobValue sourceBytesBlob)
        {
            return null;
        }

        if (KernelFunction.SignedIntegerFromValueRelaxed(offsetArg) is not { } offset)
        {
            return null;
        }

        var offsetInt = (int)offset;

        while (offsetInt < sourceBytesBlob.Bytes.Length - 3)
        {
            var nextChar =
                BinaryPrimitives.ReadInt32BigEndian(sourceBytesBlob.Bytes.Span[offsetInt..]);

            if (nextChar is '"')
            {
                break;
            }

            if (nextChar is '\\')
            {
                break;
            }

            offsetInt += 4;
        }

        return
            () => new PrecompiledResult.FinalValue(
                IntegerEncoding.EncodeSignedInteger(offsetInt),
                StackFrameCount: 0);
    }

    private static readonly PineValue IntegerOneValue =
        IntegerEncoding.EncodeSignedInteger(1);

    private static readonly PineValue Tag_BlobValue_Value =
        StringEncoding.ValueFromString("BlobValue");

    private static readonly PineValue Tag_ListValue_Value =
        StringEncoding.ValueFromString("ListValue");

    private static readonly PineValue Tag_String_Value =
        StringEncoding.ValueFromString("String");

    private static readonly PineValue Tag_Nothing_Name_Value =
        StringEncoding.ValueFromString("Nothing");

    private static readonly PineValue Tag_Just_Name_Value =
        StringEncoding.ValueFromString("Just");

    private static readonly PineValue Tag_Branch2_Name_Value =
        StringEncoding.ValueFromString("Branch2");

    private static readonly PineValue Tag_PState_Name_Value =
        StringEncoding.ValueFromString("PState");

    private static readonly PineValue Tag_EQ_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("EQ", []));

    private static readonly PineValue Tag_LT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LT", []));

    private static readonly PineValue Tag_GT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("GT", []));

    private static readonly PineValue Tag_Nothing_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Nothing", []));

    private static readonly PineValue Tag_BE_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("BE", []));

    private static readonly PineValue Tag_LE_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LE", []));

    private static readonly PineValue Tag_I8_Name_Value =
        StringEncoding.ValueFromString("I8");

    private static readonly PineValue Tag_I16_Name_Value =
        StringEncoding.ValueFromString("I16");

    private static readonly PineValue Tag_I32_Name_Value =
        StringEncoding.ValueFromString("I32");

    private static readonly PineValue Tag_U8_Name_Value =
        StringEncoding.ValueFromString("U8");

    private static readonly PineValue Tag_U16_Name_Value =
        StringEncoding.ValueFromString("U16");

    private static readonly PineValue Tag_U32_Name_Value =
        StringEncoding.ValueFromString("U32");

    private static readonly PineValue Tag_SequenceEncoder_Name_Value =
        StringEncoding.ValueFromString("SequenceEncoder");

    private static readonly PineValue Tag_BytesEncoder_Name_Value =
        StringEncoding.ValueFromString("BytesEncoder");


    private static readonly PineValue Character_ASCII_Newline_Value =
        PineValue.Blob([0, 0, 0, 10]);

    private static PineValue.ListValue Tag_Just_Value(PineValue justValue) =>
        PineValue.List(
            [
            StringEncoding.ValueFromString("Just"),
            PineValue.List([justValue])
            ]
        );

    private static readonly IImmutableDictionary<string, Expression> popularExpressionDictionary =
        PopularExpression.BuildPopularExpressionDictionary();

    private static readonly IImmutableDictionary<string, PineValue> popularValueDictionary =
        PopularExpression.BuildPopularValueDictionary();

    private static readonly FrozenDictionary<Expression, IReadOnlyList<PrecompiledEntry>> PrecompiledDict =
        BuildPrecompiled()
        .GroupBy(kv => kv.Key)
        .ToFrozenDictionary(
            keySelector:
            group => group.Key,
            elementSelector:
            group =>
            (IReadOnlyList<PrecompiledEntry>)
            [..group
            .SelectMany(kv => kv.Value)
            .OrderByDescending(entry => entry.EnvConstraint.ParsedItems.Count)]);

}
