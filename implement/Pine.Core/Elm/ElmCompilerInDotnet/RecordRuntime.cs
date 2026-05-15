using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Contains runtime functions for record operations when field layout is not known at compile time.
/// These functions iterate through record fields at runtime to find or update fields by name.
/// When the record type is known at compile time, the compiler uses direct index-based access instead.
/// <para>
/// For more details on how we compile Elm code using records, see:
/// <see href="https://github.com/pine-vm/pine/blob/aa5acc2131910f90a8b61437f8a590036fb7f097/implement/Pine.Core/Elm/ElmCompilerInDotnet/elm-compiler-implementation-guide.md#record-access-and-record-update"></see>
/// </para>
/// </summary>
public static class RecordRuntime
{
    /// <summary>
    /// A Pine function that updates fields in a record.
    /// Takes environment: [record, updates] where updates is a list of [fieldName, newValue] pairs.
    /// Both the record fields and updates must be sorted alphabetically by field name.
    /// </summary>
    public static readonly PineValue PineFunctionForRecordUpdateAsValue =
        ExpressionEncoding.EncodeExpressionAsValue(BuildPineFunctionForRecordUpdate());

    /// <summary>
    /// A Pine function that accesses a field in a record by name.
    /// Takes environment: [record, fieldName] and returns the field value.
    /// </summary>
    public static readonly PineValue PineFunctionForRecordAccessAsValue =
        ExpressionEncoding.EncodeExpressionAsValue(BuildPineFunctionForRecordAccess());

    /// <summary>
    /// Builds the Pine expression for record update.
    /// The function takes [record, updates] and returns a new record with updated fields.
    /// </summary>
    private static Expression BuildPineFunctionForRecordUpdate()
    {
        // Environment layout: [record, updates]
        // record is the new flat layout: [tag, name0, value0, name1, value1, ...]
        // updates is: [[fieldName1, newValue1], [fieldName2, newValue2], ...]

        var recordExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [0],
                Expression.EnvironmentInstance);

        var updatesExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1],
                Expression.EnvironmentInstance);

        // The record's tag is its first item; the field stream starts at offset 1
        // and is [name0, value0, name1, value1, ...].
        var recordTagExpr = BuiltinHelpers.ApplyBuiltinHead(recordExpr);

        var recordFieldsExpr = BuiltinHelpers.ApplyBuiltinSkip(1, recordExpr);

        // Build the recursive function to update fields
        var recursiveFunctionExpr = BuildRecursiveFieldUpdateFunction();

        var recursiveFunctionValue = ExpressionEncoding.EncodeExpressionAsValue(recursiveFunctionExpr);

        // Call the recursive function with: [self, updates, [], fields]
        // `processed` (initially empty) and `remaining` (initially the field stream) are
        // both flat lists [name, value, name, value, ...].
        var recursiveCallEnv =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(recursiveFunctionValue),
                updatesExpr,
                Expression.EmptyList, // processed flat fields (initially empty)
                recordFieldsExpr // remaining flat fields
                ]);

        var updatedFieldsExpr =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(recursiveFunctionValue),
                environment: recursiveCallEnv);

        // Reconstruct the record by prepending the tag to the flat updated-field stream:
        // concat([tag], updatedFields)
        return
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.concat),
                Expression.ListInstance(
                    [
                    Expression.ListInstance([recordTagExpr]),
                    updatedFieldsExpr
                    ]));
    }

    /// <summary>
    /// Builds a recursive function that iterates through record fields and updates matching ones.
    /// Environment layout: [self, updates, processedFields, remainingFields]
    /// 
    /// Both <c>processedFields</c> and <c>remainingFields</c> are flat lists in the new
    /// record layout: <c>[name0, value0, name1, value1, ...]</c>.
    /// 
    /// IMPORTANT: Both <c>remainingFields</c> (read in pairs) and <c>updates</c> must be sorted
    /// alphabetically by field name. The algorithm relies on this ordering to efficiently merge
    /// the two streams in a single pass:
    /// - When names match: use the update value
    /// - When names don't match: the current field name must be less than the update field name
    ///   (since updates only contain valid field names from the record)
    /// </summary>
    private static Expression BuildRecursiveFieldUpdateFunction()
    {
        // Environment: [self, updates, processed, remaining]
        var selfExpr = ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);

        var updatesExpr = ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);
        var processedExpr = ExpressionBuilder.BuildExpressionForPathInExpression([2], Expression.EnvironmentInstance);
        var remainingExpr = ExpressionBuilder.BuildExpressionForPathInExpression([3], Expression.EnvironmentInstance);

        // Check if remaining fields is empty
        var remainingIsEmpty =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                remainingExpr,
                Expression.EmptyList);

        // Base case: no more fields to process, return the processed list
        // (remaining is empty at this point, so we just return processed)
        var baseCase = processedExpr;

        // Read the first (name, value) pair from the flat remaining stream.
        var firstFieldNameExpr = BuiltinHelpers.ApplyBuiltinHead(remainingExpr);

        var firstFieldValueExpr =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(1, remainingExpr));

        // Drop both the name and the value: remainingFields advances 2 items at a time.
        var restRemainingExpr = BuiltinHelpers.ApplyBuiltinSkip(2, remainingExpr);

        // Check if updates list is empty
        var updatesIsEmpty =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                updatesExpr,
                Expression.EmptyList);

        // Get the first update [updateName, newValue]
        var firstUpdateExpr = BuiltinHelpers.ApplyBuiltinHead(updatesExpr);

        var firstUpdateNameExpr = BuiltinHelpers.ApplyBuiltinHead(firstUpdateExpr);

        var firstUpdateValueExpr =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(1, firstUpdateExpr));

        // Check if field name matches update name
        var namesMatch =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                firstFieldNameExpr,
                firstUpdateNameExpr);

        // If names match: use the update value instead of original field
        // newProcessed = concat(processed, [originalName, newValue])
        // newUpdates = skip 1 updates (consume the update)
        // recurse with [self, newUpdates, newProcessed, restRemaining]
        var processedWithUpdate =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.concat),
                Expression.ListInstance(
                    [
                    processedExpr,
                    Expression.ListInstance([firstFieldNameExpr, firstUpdateValueExpr])
                    ]));

        var restUpdates = BuiltinHelpers.ApplyBuiltinSkip(1, updatesExpr);

        var recurseAfterMatch =
            new Expression.ParseAndEval(
                encoded: selfExpr,
                environment: Expression.ListInstance(
                    [
                    selfExpr,
                    restUpdates,
                    processedWithUpdate,
                    restRemainingExpr
                    ]));

        // If names don't match: keep original field
        // newProcessed = concat(processed, [originalName, originalValue])
        // recurse with [self, updates, newProcessed, restRemaining]
        var processedWithOriginal =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.concat),
                Expression.ListInstance(
                    [
                    processedExpr,
                    Expression.ListInstance([firstFieldNameExpr, firstFieldValueExpr])
                    ]));

        var recurseWithOriginal =
            new Expression.ParseAndEval(
                encoded: selfExpr,
                environment: Expression.ListInstance(
                    [
                    selfExpr,
                    updatesExpr,
                    processedWithOriginal,
                    restRemainingExpr
                    ]));

        // Decision logic:
        // if updates is empty: keep original field and continue
        // else if names match: use update and continue
        // else: keep original and continue (field name < update name)
        var fieldUpdateLogic =
            Expression.ConditionalInstance(
                condition: updatesIsEmpty,
                trueBranch: recurseWithOriginal, // no more updates, keep original
                falseBranch: Expression.ConditionalInstance(
                    condition: namesMatch,
                    trueBranch: recurseAfterMatch, // use update
                    falseBranch: recurseWithOriginal)); // keep original (field comes before update alphabetically)

        // Full function: if remaining is empty, return processed; else process next field
        return
            Expression.ConditionalInstance(
                condition: remainingIsEmpty,
                trueBranch: baseCase,
                falseBranch: fieldUpdateLogic);
    }

    /// <summary>
    /// Builds the Pine expression for record field access.
    /// The function takes [record, fieldName] and returns the value of the specified field.
    /// </summary>
    private static Expression BuildPineFunctionForRecordAccess()
    {
        // Environment layout: [record, fieldName]
        // record is the new flat layout: [tag, name0, value0, name1, value1, ...].
        // fieldName is a string value.

        var recordExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [0],
                Expression.EnvironmentInstance);

        var fieldNameExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1],
                Expression.EnvironmentInstance);

        // Drop the record tag; remaining is the flat field stream [name, value, name, value, ...].
        var recordFieldsExpr = BuiltinHelpers.ApplyBuiltinSkip(1, recordExpr);

        // Build the recursive function to look up the field
        var recursiveFunctionExpr = BuildRecursiveFieldLookupFunction();

        var recursiveFunctionValue = ExpressionEncoding.EncodeExpressionAsValue(recursiveFunctionExpr);

        // Call the recursive function with: [self, fieldName, fields]
        var recursiveCallEnv =
            Expression.ListInstance(
                [
                Expression.LiteralInstance(recursiveFunctionValue),
                fieldNameExpr,
                recordFieldsExpr
                ]);

        return
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(recursiveFunctionValue),
                environment: recursiveCallEnv);
    }

    /// <summary>
    /// Builds a recursive function that looks up a field by name in a flat record-field stream.
    /// Environment layout: [self, fieldName, remainingFields]
    /// where <c>remainingFields</c> is <c>[name0, value0, name1, value1, ...]</c>.
    /// Returns the fieldValue when a match is found.
    /// </summary>
    private static Expression BuildRecursiveFieldLookupFunction()
    {
        // Environment: [self, fieldName, remaining]
        var selfExpr = ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);

        var targetFieldNameExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);

        var remainingExpr = ExpressionBuilder.BuildExpressionForPathInExpression([2], Expression.EnvironmentInstance);

        // Check if remaining fields is empty - this shouldn't happen for valid field access
        var remainingIsEmpty =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                remainingExpr,
                Expression.EmptyList);

        // Read the first (name, value) pair from the flat remaining stream.
        var firstFieldNameExpr = BuiltinHelpers.ApplyBuiltinHead(remainingExpr);

        var firstFieldValueExpr =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(1, remainingExpr));

        // Check if field names match
        var namesMatch =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                firstFieldNameExpr,
                targetFieldNameExpr);

        // Advance two items: drop both the name and the value.
        var restRemainingExpr = BuiltinHelpers.ApplyBuiltinSkip(2, remainingExpr);

        var recurseExpr =
            new Expression.ParseAndEval(
                encoded: selfExpr,
                environment: Expression.ListInstance(
                    [
                    selfExpr,
                    targetFieldNameExpr,
                    restRemainingExpr
                    ]));

        // If names match: return the field value
        // If names don't match: continue searching
        var searchLogic =
            Expression.ConditionalInstance(
                condition: namesMatch,
                trueBranch: firstFieldValueExpr,
                falseBranch: recurseExpr);

        // If remaining is empty, we didn't find the field (shouldn't happen in valid Elm)
        // Return empty list as error indicator
        return
            Expression.ConditionalInstance(
                condition: remainingIsEmpty,
                trueBranch: Expression.EmptyList, // Field not found (error case)
                falseBranch: searchLogic);
    }
}
