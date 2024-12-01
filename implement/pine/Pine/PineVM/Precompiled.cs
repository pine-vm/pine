using ElmTime.ElmInteractive;
using Pine.Core;
using Pine.ElmInteractive;
using System;
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
        EnvConstraintId EnvConstraint,
        Func<PineValue, PineVMParseCache, Func<PrecompiledResult>?> PrecompiledDelegate)
    {
        public PrecompiledEntry(
            EnvConstraintId EnvConstraint,
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
            EnvConstraintId EnvConstraint,
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

        var stringAnyExposedValue = popularValueDictionary["String.any.exposed"];

        var stringSliceExpression = popularExpressionDictionary["String.slice"];

        var stringAnySliceExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(stringSliceExpression);

        var stringSliceExposedValue = popularValueDictionary["String.slice.exposed"];

        var dictGetExpression = popularExpressionDictionary["dictGet"];

        var dictGetExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictGetExpression);

        var dictGetAfterCompareExpression = popularExpressionDictionary["dictGetAfterCompare"];

        var dictGetAfterCompareExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictGetAfterCompareExpression);


        var compareExposedValue = popularValueDictionary["Basics.compare.exposed"];


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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
            var listAllEnvClass =
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
            var stringSliceEnvClass =
                EnvConstraintId.Create(
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
            var assocListGetExpressionEnvClass =
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    adaptivePartialApplicationExpressionValue),

                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [1,0],
                    PineValueAsString.ValueFromString("Function")),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    adaptivePartialApplicationExpression,
                    [new PrecompiledEntry(adaptivePartialApplicationEnvClass, ElmCompiledAdaptivePartialApplication)]);
        }


        {
            var countPineValueContentEnvClass =
                EnvConstraintId.Create(
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
            var unsafeUnsignedBlobValueFromIntExpression =
                popularExpressionDictionary["Pine.unsafeUnsignedBlobValueFromInt"];

            var unsafeUnsignedBlobValueFromIntExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(unsafeUnsignedBlobValueFromIntExpression);

            var valueFromCharExpression =
                popularExpressionDictionary["Pine.valueFromChar"];

            var valueFromCharExpressionValue =
                ExpressionEncoding.EncodeExpressionAsValue(valueFromCharExpression);

            var computeValueFromStringRecursiveExpr =
                popularExpressionDictionary["Pine.computeValueFromStringRecursive"];

            var computeValueFromStringRecursiveExprValue =
                ExpressionEncoding.EncodeExpressionAsValue(computeValueFromStringRecursiveExpr);

            var envClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0],
                        PineValue.List(
                            [
                            valueFromCharExpressionValue,
                            unsafeUnsignedBlobValueFromIntExpressionValue,
                            computeValueFromStringRecursiveExprValue
                            ])),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    computeValueFromStringRecursiveExpr,
                    [new PrecompiledEntry(
                        envClass,
                        PineComputeValueFromStringRecursive)]);
        }


        {
            var countOffsetsInStringEnvClass =
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 0],
                        adaptivePartialApplicationExpressionValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 1],
                        elmKernelParser_chompWhileHelpExpressionValue),
                ]);

            EnvConstraintId chompWhileHelpEnvClassFromPredicate(PineValue predicateValue)
            {
                return
                    EnvConstraintId.Create(
                        [..chompWhileHelpEnvClass.ParsedEnvItems
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
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 1],
                        stringSliceExposedValue),
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0, 2],
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
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0],
                        PineValue.List(
                            [
                            stringAnyExposedValue,
                            stringSliceExposedValue,
                            skipWhileWithoutLinebreakHelpExpressionValue,
                            ])),
                    ]);

            EnvConstraintId envClassFromPredicate(PineValue predicateValue)
            {
                return
                    EnvConstraintId.Create(
                        [..skipWhileWithoutLinebreakHelpEnvClass.ParsedEnvItems
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
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                        [0],
                        PineValue.List(
                            [
                            stringAnyExposedValue,
                            stringSliceExposedValue,
                            skipWhileHelpExpressionValue,
                            ])),
                    ]);

            EnvConstraintId envClassFromPredicate(PineValue predicateValue)
            {
                return
                    EnvConstraintId.Create(
                        [..skipWhileHelpEnvClass.ParsedEnvItems
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
                        EnvConstraintId.Create([]),
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
                EnvConstraintId.Create(
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
                EnvConstraintId.Create(
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

        if (charValue is not PineValue.BlobValue charBlob || charBlob.Bytes.Length is not 1)
        {
            return PineVMValues.FalseValue;
        }

        var charCode = charBlob.Bytes.Span[0];

        var isLower =
            0x61 <= charCode &&
            charCode <= 0x7A;

        return
            isLower
            ?
            PineVMValues.TrueValue
            :
            PineVMValues.FalseValue;
    }

    static PineValue CharIsUpper(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var charValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        if (charValue is not PineValue.BlobValue charBlob || charBlob.Bytes.Length is not 1)
        {
            return PineVMValues.FalseValue;
        }

        var charCode = charBlob.Bytes.Span[0];

        var isUpper =
            0x41 <= charCode &&
            charCode <= 0x5A;

        return
            isUpper
            ?
            PineVMValues.TrueValue
            :
            PineVMValues.FalseValue;
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

            var leftProduct = KernelFunction.int_mul(numA, denomB);
            var rightProduct = KernelFunction.int_mul(numB, denomA);

            if (leftProduct == rightProduct)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([leftProduct, rightProduct])) == PineVMValues.TrueValue)
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

            var rightProduct = KernelFunction.int_mul(denomA, b);

            if (numA == rightProduct)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([numA, rightProduct])) == PineVMValues.TrueValue)
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

            var leftProduct = KernelFunction.int_mul(a, denomB);

            if (leftProduct == numB)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([leftProduct, numB])) == PineVMValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (a is PineValue.ListValue)
        {
            return CompareLists(a, b);
        }

        if (KernelFunction.int_is_sorted_asc(PineValue.List([a, b])) == PineVMValues.TrueValue)
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

        if (a is PineValue.ListValue listA && 0 < listA.Elements.Count)
        {
            if (b == PineValue.EmptyList)
            {
                return Tag_GT_Value;
            }

            if (b is PineValue.ListValue listB && 0 < listB.Elements.Count)
            {
                var commonLength =
                    listA.Elements.Count < listB.Elements.Count ?
                    listA.Elements.Count :
                    listB.Elements.Count;

                for (var i = 0; i < commonLength; ++i)
                {
                    var itemA = listA.Elements[i];
                    var itemB = listB.Elements[i];

                    var itemOrder = BasicsCompare(itemA, itemB);

                    if (itemOrder != Tag_EQ_Value)
                    {
                        return itemOrder;
                    }
                }

                if (listA.Elements.Count < listB.Elements.Count)
                {
                    return Tag_LT_Value;
                }

                if (listA.Elements.Count > listB.Elements.Count)
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
        if (stringA is PineValue.ListValue listA && stringB is PineValue.ListValue listB)
        {
            var commonLength =
                listA.Elements.Count < listB.Elements.Count ?
                listA.Elements.Count :
                listB.Elements.Count;

            for (var i = 0; i < commonLength; ++i)
            {
                var itemA = listA.Elements[i];
                var itemB = listB.Elements[i];

                if (itemA is not PineValue.BlobValue blobA)
                {
                    return PineValue.EmptyList;
                }

                if (itemB is not PineValue.BlobValue blobB)
                {
                    return PineValue.EmptyList;
                }

                var charA = PineValueAsInteger.UnsignedIntegerFromBlobValue(blobA.Bytes.Span);
                var charB = PineValueAsInteger.UnsignedIntegerFromBlobValue(blobB.Bytes.Span);

                if (charA < charB)
                {
                    return Tag_LT_Value;
                }

                if (charA > charB)
                {
                    return Tag_GT_Value;
                }
            }

            if (listA.Elements.Count < listB.Elements.Count)
            {
                return Tag_LT_Value;
            }

            if (listA.Elements.Count > listB.Elements.Count)
            {
                return Tag_GT_Value;
            }

            return Tag_EQ_Value;
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
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
            isEq ? PineVMValues.TrueValue : PineVMValues.FalseValue,
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
            aTagArgs is PineValue.ListValue argsList && argsList.Elements.Count is 2)
        {
            var numAValue = argsList.Elements[0];
            var denAValue = argsList.Elements[1];

            return (numAValue == b && denAValue == IntegerOneValue, 0);
        }

        if (bTag == ElmValue.ElmFloatTypeTagNameAsValue &&
            bTagArgs is PineValue.ListValue argsListB && argsListB.Elements.Count is 2)
        {
            var numBValue = argsListB.Elements[0];
            var denBValue = argsListB.Elements[1];

            return (a == numBValue && IntegerOneValue == denBValue, 0);
        }

        if (a is PineValue.BlobValue)
        {
            return (false, 0);
        }

        {
            if (a is PineValue.ListValue listA && b is PineValue.ListValue listB)
            {
                if (listA.Elements.Count != listB.Elements.Count)
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
                        (PineValue.List(dictAList) == PineValue.List(dictBList), dictAList.Count + dictBList.Count);
                }

                if (aTag == ElmValue.ElmSetTypeTagNameAsValue)
                {
                    var dictA = PineVM.ValueFromPathInValueOrEmptyList(a, [1, 0]);
                    var dictB = PineVM.ValueFromPathInValueOrEmptyList(b, [1, 0]);

                    var dictAKeys = DictKeysRecursive(dictA);
                    var dictBKeys = DictKeysRecursive(dictB);

                    return
                        (PineValue.List(dictAKeys) == PineValue.List(dictBKeys), dictAKeys.Count + dictBKeys.Count);
                }

                return ListsEqualRecursive(listA.Elements, listB.Elements);
            }
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static IReadOnlyList<PineValue> DictToListRecursive(PineValue dict)
    {
        var tag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

        if (tag == ElmValue.ElmDictEmptyTagNameAsValue)
        {
            return [];
        }

        if (tag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

            var argKey = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [1]);
            var argValue = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [2]);
            var argLeft = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
            var argRight = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

            return
                [
                ..DictToListRecursive(argLeft),
                PineValue.List([argKey, argValue]),
                ..DictToListRecursive(argRight)
                ];
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static IReadOnlyList<PineValue> DictKeysRecursive(PineValue dict)
    {
        var tag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

        if (tag == ElmValue.ElmDictEmptyTagNameAsValue)
        {
            return [];
        }

        if (tag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

            var argKey = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [1]);

            var argLeft = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
            var argRight = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

            return
                [
                ..DictKeysRecursive(argLeft),
                argKey,
                ..DictKeysRecursive(argRight)
                ];
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static (bool, int) ListsEqualRecursive(
        IReadOnlyList<PineValue> listA,
        IReadOnlyList<PineValue> listB)
    {
        int totalCount = 0;

        for (int i = 0; i < listA.Count; i++)
        {
            var (itemEq, itemCount) = BasicsEqRecursive(listA[i], listB[i]);

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
            return ListMember(item, listValue.Elements);
        }

        return new PrecompiledResult.FinalValue(
            PineVMValues.FalseValue,
            StackFrameCount: 0);
    }

    static PrecompiledResult.FinalValue ListMember(PineValue item, IReadOnlyList<PineValue> list)
    {
        int totalCount = 0;

        for (var i = 0; i < list.Count; ++i)
        {
            var (itemEq, itemEqStackFrameCount) = BasicsEqRecursive(item, list[i]);

            totalCount += itemEqStackFrameCount;

            if (itemEq)
            {
                return new PrecompiledResult.FinalValue(
                    PineVMValues.TrueValue,
                    StackFrameCount: totalCount);
            }
        }

        return new PrecompiledResult.FinalValue(
            PineVMValues.FalseValue,
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
            int totalCount = 0;

            for (var i = 0; i < listValue.Elements.Count; ++i)
            {
                if (listValue.Elements[i] is PineValue.ListValue itemList && 1 < itemList.Elements.Count)
                {
                    var (itemEq, itemEqStackFrameCount) = BasicsEqRecursive(key, itemList.Elements[0]);

                    totalCount += itemEqStackFrameCount;

                    if (itemEq)
                    {
                        return new PrecompiledResult.FinalValue(
                            Tag_Just_Value(itemList.Elements[1]),
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

        for (var i = 0; i < listValue.Elements.Count; ++i)
        {
            if (listValue.Elements[i] is PineValue.ListValue itemList)
            {
                var candidateKey =
                    itemList.Elements.Count < 1
                    ?
                    PineValue.EmptyList
                    :
                    itemList.Elements[0];

                var (itemEq, itemEqStackFrameCount) =
                    BasicsEqRecursive(key, candidateKey);

                if (itemEq)
                {
                    var itemListValue =
                        itemList.Elements.Count < 2
                        ?
                        PineValue.EmptyList
                        :
                        itemList.Elements[1];

                    return
                        new PrecompiledResult.FinalValue(
                            Tag_Just_Value(
                                PineValue.List(
                                [
                                    KernelFunction.int_add(index, PineValueAsInteger.ValueFromSignedInteger(i)),
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

        var newUnique = new List<PineValue>(accumulatorList.Elements);

        for (var i = 0; i < remainingList.Elements.Count; ++i)
        {
            var item = remainingList.Elements[i];

            var (isMember, _) = ListMember(item, newUnique);

            if (isMember != PineVMValues.TrueValue)
            {
                newUnique.Add(item);
            }
        }

        return new PrecompiledResult.FinalValue(
            PineValue.List([.. accumulatorList.Elements, .. newUnique]),
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

        var countValue = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var count =
            PineValueAsInteger.SignedIntegerFromValueStrict(countValue)
            .Extract(err => throw new Exception(err));

        var dict = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return PineValueAsInteger.ValueFromSignedInteger(sizeHelp((long)count, dict));
    }


    static PineValue ElmCompiledRecordAccess(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var fieldNameValue = PineVM.ValueFromPathInValueOrEmptyList(environment, [1]);
        var remainingFieldsValue = PineVM.ValueFromPathInValueOrEmptyList(environment, [2]);

        if (remainingFieldsValue is PineValue.ListValue remainingFields)
        {
            for (var i = 0; i < remainingFields.Elements.Count; ++i)
            {
                var field = remainingFields.Elements[i];

                if (field is PineValue.ListValue fieldList && 0 < fieldList.Elements.Count)
                {
                    if (fieldList.Elements[0] == fieldNameValue)
                    {
                        if (1 < fieldList.Elements.Count)
                        {
                            return fieldList.Elements[1];
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
                PineValueAsInteger.ValueFromSignedInteger(nodeCount),
                PineValueAsInteger.ValueFromSignedInteger(byteCount)
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

        if (listValue.Elements.Count is not 2)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        if (listValue.Elements[1] is not PineValue.ListValue tagArgumentsList)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        if (tagArgumentsList.Elements.Count is not 1)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        var tagArgument = tagArgumentsList.Elements[0];

        if (listValue.Elements[0] == Tag_ListValue_Value &&
            tagArgument is PineValue.ListValue listValueListList)
        {
            long nodeCount = 0;
            long byteCount = 0;

            for (var i = 0; i < listValueListList.Elements.Count; ++i)
            {
                var (itemNodeCount, itemByteCount) =
                    CountEncodedPineValueContentRecursive(listValueListList.Elements[i]);

                nodeCount += itemNodeCount + 1;
                byteCount += itemByteCount;
            }

            return (nodeCount, byteCount);
        }

        if (listValue.Elements[0] == Tag_BlobValue_Value &&
            tagArgument is PineValue.ListValue blobValueList)
        {
            return (0, blobValueList.Elements.Count);
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PrecompiledResult.FinalValue? PineComputeValueFromStringRecursive(
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
            new PineValue[mappedCharsList.Elements.Count + stringCharsList.Elements.Count];

        for (var i = 0; i < mappedCharsList.Elements.Count; ++i)
        {
            mappedValues[i] = mappedCharsList.Elements[i];
        }

        for (var i = 0; i < stringCharsList.Elements.Count; ++i)
        {
            if (stringCharsList.Elements[i] is not PineValue.BlobValue charBlobValue)
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

            mappedValues[mappedCharsList.Elements.Count + i] = blobValue;
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

    static PrecompiledResult.FinalValue? ElmKernelParser_countOffsetsInString(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0, 0])).IsOkOrNullable() is not { } offset)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0, 1])).IsOkOrNullable() is not { } newlines)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0, 2])).IsOkOrNullable() is not { } col)
        {
            return null;
        }

        if (PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1, 0]) is not PineValue.ListValue charsList)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1, 1])).IsOkOrNullable() is not { } end)
        {
            return null;
        }

        if (offset < 0 || offset >= charsList.Elements.Count || end > charsList.Elements.Count)
        {
            return null;
        }

        for (var i = (int)offset; i < end; ++i)
        {
            if (charsList.Elements[i] == Character_ASCII_Newline_Value)
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
                PineValueAsInteger.ValueFromSignedInteger(newlines),
                PineValueAsInteger.ValueFromSignedInteger(col)
                ]);

        return
            new PrecompiledResult.FinalValue(
                finalValue,
                StackFrameCount: 1 + charsList.Elements.Count);
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

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1])).IsOkOrNullable() is not { } offset)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2])).IsOkOrNullable() is not { } row)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3])).IsOkOrNullable() is not { } col)
        {
            return null;
        }

        if (PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]) is not PineValue.ListValue bigCharsList)
        {
            return null;
        }

        int newOffset = (int)offset;

        for (; newOffset < bigCharsList.Elements.Count; ++newOffset)
        {
            if (bigCharsList.Elements.Count <= newOffset + patternList.Elements.Count)
            {
                newOffset = -1;
                break;
            }

            bool found = true;

            for (var j = 0; j < patternList.Elements.Count; ++j)
            {
                if (patternList.Elements[j] != bigCharsList.Elements[newOffset + j])
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

        int newRow = (int)row;
        int newCol = (int)col;

        for (var i = (int)offset; i < offset + consumedLength; ++i)
        {
            if (bigCharsList.Elements[i] == Character_ASCII_Newline_Value)
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
                PineValueAsInteger.ValueFromSignedInteger(newOffset),
                PineValueAsInteger.ValueFromSignedInteger(newRow),
                PineValueAsInteger.ValueFromSignedInteger(newCol)
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

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(offsetValue).IsOkOrNullable() is not { } startOffset)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(rowValue).IsOkOrNullable() is not { } row)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(colValue).IsOkOrNullable() is not { } col)
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

        int offset = (int)startOffset;

        while (true)
        {
            if (srcCharsList.Elements.Count <= offset)
            {
                break;
            }

            var currentChar = srcCharsList.Elements[offset];

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
                PineValueAsInteger.ValueFromSignedInteger(offset),
                PineValueAsInteger.ValueFromSignedInteger(row),
                PineValueAsInteger.ValueFromSignedInteger(col)
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

        if (remainingItemsListValue.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(mappedReversed, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.functionParameterCount -
            functionRecordOk.Value.argumentsAlreadyCollected.Count;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsList =
                PineValue.List([.. functionRecordOk.Value.argumentsAlreadyCollected, itemValue]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemsResults = new PineValue[remainingItemsListValue.Elements.Count];
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            itemsResults[itemIndex] = itemResultValue;

            ++itemIndex;

            if (itemIndex < remainingItemsListValue.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(remainingItemsListValue.Elements[itemIndex]),
                        Callback: step);
            }

            var resultValue =
                KernelFunction.concat([mappedReversed, PineValue.List(itemsResults)]);

            return new PineVM.ApplyStepwise.StepResult.Complete(resultValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(remainingItemsListValue.Elements[itemIndex]),
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

        if (itemsListValue.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(PineValue.EmptyList, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        if (functionRecordOk.Value.functionParameterCount is not 1)
        {
            return null;
        }

        if (functionRecordOk.Value.argumentsAlreadyCollected.Count is not 0)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsList = PineValue.List([itemValue]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemsResults = new PineValue[itemsListValue.Elements.Count];
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            itemsResults[itemIndex] = itemResultValue;

            ++itemIndex;

            if (itemIndex < itemsListValue.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
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
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
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

        if (itemsListValue.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(argumentAggregate, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.functionParameterCount -
            functionRecordOk.Value.argumentsAlreadyCollected.Count;

        if (functionRecordRemainingParamCount is not 2)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue aggregate, PineValue itemValue)
        {
            var argumentsList =
                PineValue.List([.. functionRecordOk.Value.argumentsAlreadyCollected, itemValue, aggregate]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var mutatedAggregate = argumentAggregate;
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            mutatedAggregate = itemResultValue;

            ++itemIndex;

            if (itemIndex < itemsListValue.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(mutatedAggregate, itemsListValue.Elements[itemIndex]),
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
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(mutatedAggregate, itemsListValue.Elements[itemIndex]),
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

        if (itemsList.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(accumulatedReversed, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentPredicateFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.functionParameterCount -
            functionRecordOk.Value.argumentsAlreadyCollected.Count;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsList =
                PineValue.List([.. functionRecordOk.Value.argumentsAlreadyCollected, itemValue]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var includedItems = new PineValue[itemsList.Elements.Count];
        var itemIndex = 0;
        var includedItemCount = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            if (itemResultValue == PineVMValues.TrueValue)
            {
                includedItems[includedItemCount] = itemsList.Elements[itemIndex];

                ++includedItemCount;
            }

            ++itemIndex;

            if (itemIndex < itemsList.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsList.Elements[itemIndex]),
                        Callback: step);
            }

            var resultValue =
                KernelFunction.concat(
                    [accumulatedReversed,
                    PineValue.List(includedItems[..includedItemCount])]);

            return new PineVM.ApplyStepwise.StepResult.Complete(resultValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsList.Elements[itemIndex]),
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

        if (itemsListValue.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(PineVMValues.TrueValue, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.functionParameterCount -
            functionRecordOk.Value.argumentsAlreadyCollected.Count;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsList =
                PineValue.List([.. functionRecordOk.Value.argumentsAlreadyCollected, itemValue]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            if (itemResultValue != PineVMValues.TrueValue)
            {
                return new PineVM.ApplyStepwise.StepResult.Complete(PineVMValues.FalseValue);
            }

            ++itemIndex;

            if (itemIndex < itemsListValue.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
                        Callback: step);
            }

            return
                new PineVM.ApplyStepwise.StepResult.Complete(PineVMValues.TrueValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
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

        if (itemsListValue.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(PineVMValues.FalseValue, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        var functionRecordRemainingParamCount =
            functionRecordOk.Value.functionParameterCount -
            functionRecordOk.Value.argumentsAlreadyCollected.Count;

        if (functionRecordRemainingParamCount is not 1)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsList =
                PineValue.List([.. functionRecordOk.Value.argumentsAlreadyCollected, itemValue]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            if (itemResultValue == PineVMValues.TrueValue)
            {
                return new PineVM.ApplyStepwise.StepResult.Complete(PineVMValues.TrueValue);
            }

            ++itemIndex;

            if (itemIndex < itemsListValue.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
                        Callback: step);
            }

            return
                new PineVM.ApplyStepwise.StepResult.Complete(PineVMValues.FalseValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? StringSlice(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        slice : Int -> Int -> String -> String
        slice start end (String chars) =
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
                        Pine_kernel.int_add [ relativeIndex, Pine_kernel.length chars ]

                    else
                        relativeIndex

                absoluteStart : Int
                absoluteStart =
                    absoluteIndex start

                sliceLength : Int
                sliceLength =
                    Pine_kernel.int_add
                        [ absoluteIndex end
                        , Pine_kernel.int_mul [ -1, absoluteStart ]
                        ]
            in
            String
                (Pine_kernel.take
                    [ sliceLength
                    , Pine_kernel.skip [ absoluteStart, chars ]
                    ]
                )
         * */

        var startValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var endValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var charsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2, 1, 0]);

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(startValue).IsOkOrNullable() is not { } start)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(endValue).IsOkOrNullable() is not { } end)
        {
            return null;
        }

        if (charsValue is not PineValue.ListValue charsList)
        {
            return null;
        }

        int absoluteIndex(int relativeIndex)
        {
            if (relativeIndex < 0)
            {
                return relativeIndex + charsList.Elements.Count;
            }

            return relativeIndex;
        }

        var absoluteStart = absoluteIndex((int)start);

        var sliceLength = absoluteIndex((int)end) - absoluteStart;

        var sliceChars =
            charsList.Elements.Count <= absoluteStart
            ?
            PineValue.EmptyList
            :
            PineVM.FusedSkipAndTake(charsList, skipCount: absoluteStart, takeCount: sliceLength);

        var slicedStringValue =
            PineValue.List(
                [
                Tag_String_Value,
                PineValue.List([sliceChars])
                ]);

        var finalValue =
            new PrecompiledResult.FinalValue(
                slicedStringValue,
                StackFrameCount: 0);

        return () => finalValue;
    }

    static Func<PrecompiledResult>? ParserFast_skipWhileWhitespaceHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        /*
        skipWhileWhitespaceHelp : Int -> Int -> Int -> String -> Int -> State
        skipWhileWhitespaceHelp offset row col src indent =
            case String.slice offset (offset + 1) src of
                " " ->
                    skipWhileWhitespaceHelp (offset + 1) row (col + 1) src indent

                "\n" ->
                    skipWhileWhitespaceHelp (offset + 1) (row + 1) 1 src indent

                "\u{000D}" ->
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

        var srcValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        var srcCharsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3, 1, 0]);

        var indentValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]);

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(offsetValue).IsOkOrNullable() is not { } startOffset)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(rowValue).IsOkOrNullable() is not { } row)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(colValue).IsOkOrNullable() is not { } col)
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
            if (srcCharsList.Elements.Count <= offset)
            {
                break;
            }

            var currentChar = srcCharsList.Elements[offset];

            if (currentChar is not PineValue.BlobValue currentCharBlob)
            {
                break;
            }

            if (currentCharBlob.Bytes.Length is not 1)
            {
                break;
            }

            var byteValue = currentCharBlob.Bytes.Span[0];

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
                        srcValue,
                        PineValueAsInteger.ValueFromSignedInteger(offset),
                        indentValue,
                        PineValueAsInteger.ValueFromSignedInteger(row),
                        PineValueAsInteger.ValueFromSignedInteger(col),
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
        skipWhileWithoutLinebreakHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> State
        skipWhileWithoutLinebreakHelp isGood offset row col src indent =
            let
                actualChar : String
                actualChar =
                    String.slice offset (offset + 1) src
            in
            if String.any isGood actualChar then
                skipWhileWithoutLinebreakHelp isGood (offset + 1) row (col + 1) src indent

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

            if (blobValue.Bytes.Length is not 1)
            {
                return true;
            }

            var byteValue = blobValue.Bytes.Span[0];

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
        skipWhileWithoutLinebreakHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> State
        skipWhileWithoutLinebreakHelp isGood offset row col src indent =
            let
                actualChar : String
                actualChar =
                    String.slice offset (offset + 1) src
            in
            if String.any isGood actualChar then
                skipWhileWithoutLinebreakHelp isGood (offset + 1) row (col + 1) src indent

            else
                -- no match
                PState src offset indent row col
         * */

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var rowValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        var colValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        var srcValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]);

        var srcCharsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4, 1, 0]);

        var indentValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 5]);

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(offsetValue).IsOkOrNullable() is not { } startOffset)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(colValue).IsOkOrNullable() is not { } col)
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
            if (srcCharsList.Elements.Count <= offset)
            {
                break;
            }

            var currentChar = srcCharsList.Elements[offset];

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
                        srcValue,
                        PineValueAsInteger.ValueFromSignedInteger(offset),
                        indentValue,
                        rowValue,
                        PineValueAsInteger.ValueFromSignedInteger(col),
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
            PineValue.Blob([(byte)'{']);

        var closeChar =
            PineValue.Blob([(byte)'-']);

        return
            ParserFast_skipWhileHelp(
                environment,
                charValuePredicate:
                c => !(c == openChar || c == closeChar));
    }

    static PrecompiledResult.FinalValue? ParserFast_skipWhileHelp(
        PineValue environment,
        Func<PineValue, bool> charValuePredicate)
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

         * 
         * */

        var offsetValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var rowValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        var colValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 3]);

        var srcValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4]);

        var srcCharsValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 4, 1, 0]);

        var indentValue =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 5]);

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(offsetValue).IsOkOrNullable() is not { } startOffset)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(rowValue).IsOkOrNullable() is not { } row)
        {
            return null;
        }

        if (PineValueAsInteger.SignedIntegerFromValueRelaxed(colValue).IsOkOrNullable() is not { } col)
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

        int offset = (int)startOffset;

        while (true)
        {
            if (srcCharsList.Elements.Count <= offset)
            {
                break;
            }

            var currentChar = srcCharsList.Elements[offset];

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
                    srcValue,
                    PineValueAsInteger.ValueFromSignedInteger(offset),
                    indentValue,
                    PineValueAsInteger.ValueFromSignedInteger(row),
                    PineValueAsInteger.ValueFromSignedInteger(col),
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

        if (envList.Elements.Count < 2)
            return null;

        if (envList.Elements[1] is not PineValue.ListValue taggedFunctionRecordList)
            return null;

        if (envList.Elements[2] is not PineValue.ListValue newArgumentsList)
            return null;

        if (newArgumentsList.Elements.Count is 0)
        {
            return () => new PrecompiledResult.FinalValue(taggedFunctionRecordList, StackFrameCount: 0);
        }

        if (taggedFunctionRecordList.Elements.Count < 2)
            return null;

        /*
         * We already check this condition via the environment value class.
         * 
        if (taggedFunctionRecordList.Elements[0] != ElmCompilerFunctionTagValue)
            return null;
        */

        if (taggedFunctionRecordList.Elements[1] is not PineValue.ListValue functionRecord)
            return null;

        if (functionRecord.Elements.Count < 4)
            return null;

        if (functionRecord.Elements[3] is not PineValue.ListValue argsCollectedPreviouslyList)
            return null;

        if (functionRecord.Elements[1] is not PineValue.BlobValue paramCountBlob)
            return null;

        if (paramCountBlob.Bytes.Length is not 2)
            return null;

        if (paramCountBlob.Bytes.Span[0] is not 4)
            return null;

        var paramCount = paramCountBlob.Bytes.Span[1];

        var envFunctions = functionRecord.Elements[2];

        if (paramCount != argsCollectedPreviouslyList.Elements.Count + newArgumentsList.Elements.Count)
            return null;

        PineValue? combinedArgumentsValue = null;

        if (argsCollectedPreviouslyList.Elements.Count is 0)
        {
            combinedArgumentsValue = newArgumentsList;
        }
        else
        {
            var combinedArgsArray = new PineValue[argsCollectedPreviouslyList.Elements.Count + newArgumentsList.Elements.Count];

            for (var i = 0; i < argsCollectedPreviouslyList.Elements.Count; ++i)
            {
                combinedArgsArray[i] = argsCollectedPreviouslyList.Elements[i];
            }

            for (var i = 0; i < newArgumentsList.Elements.Count; ++i)
            {
                combinedArgsArray[i + argsCollectedPreviouslyList.Elements.Count] = newArgumentsList.Elements[i];
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
                ExpressionValue: functionRecord.Elements[0]);
    }

    private static readonly PineValue IntegerOneValue =
        PineValueAsInteger.ValueFromSignedInteger(1);

    private static readonly PineValue Tag_BlobValue_Value =
        PineValueAsString.ValueFromString("BlobValue");

    private static readonly PineValue Tag_ListValue_Value =
        PineValueAsString.ValueFromString("ListValue");

    private static readonly PineValue Tag_String_Value =
        PineValueAsString.ValueFromString("String");

    private static readonly PineValue Tag_Nothing_Name_Value =
        PineValueAsString.ValueFromString("Nothing");

    private static readonly PineValue Tag_Branch2_Name_Value =
        PineValueAsString.ValueFromString("Branch2");

    private static readonly PineValue Tag_PState_Name_Value =
        PineValueAsString.ValueFromString("PState");

    private static readonly PineValue Tag_EQ_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("EQ", []));

    private static readonly PineValue Tag_LT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LT", []));

    private static readonly PineValue Tag_GT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("GT", []));

    private static readonly PineValue Tag_Nothing_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Nothing", []));

    private static readonly PineValue Character_ASCII_Newline_Value =
        PineValue.Blob([10]);

    private static PineValue.ListValue Tag_Just_Value(PineValue justValue) =>
        PineValue.List(
            [
            PineValueAsString.ValueFromString("Just"),
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
            .OrderByDescending(entry => entry.EnvConstraint.ParsedEnvItems.Count)]);

}
