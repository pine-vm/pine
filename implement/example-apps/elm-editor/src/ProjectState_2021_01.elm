module ProjectState_2021_01 exposing (..)

import Bytes
import Bytes.Extra
import Diff
import List.Extra


type alias ProjectState =
    { base : String
    , differenceFromBase : ProjectStateDifference
    }


type alias ProjectStateDifference =
    { removeNodes : List (List String)
    , changeBlobs : List ( List String, List BlobChangeSequenceElement )
    }


type BlobChangeSequenceElement
    = ReuseBytes Int
    | RemoveBytes Int
    | AddBytes Bytes.Bytes


noDifference : ProjectStateDifference
noDifference =
    { removeNodes = []
    , changeBlobs = []
    }


findBlobChanges : Bytes.Bytes -> Bytes.Bytes -> List BlobChangeSequenceElement
findBlobChanges baseBlobValue blobValue =
    let
        baseBlobValueInts =
            baseBlobValue
                |> Bytes.Extra.toByteValues

        baseBlobValueBlocks =
            baseBlobValueInts
                |> splitBlobValueAtLineBreaks

        blobValueBlocks =
            blobValue
                |> Bytes.Extra.toByteValues
                |> splitBlobValueAtLineBreaks

        changes =
            Diff.diff baseBlobValueBlocks blobValueBlocks
                |> diffChangesAggregateNoChangeSequences
    in
    changes
        |> List.map
            (\change ->
                case change of
                    Diff.NoChange noChange ->
                        ReuseBytes (List.length noChange)

                    Diff.Removed removed ->
                        RemoveBytes (List.length removed)

                    Diff.Added added ->
                        AddBytes (Bytes.Extra.fromByteValues added)
            )


applyBlobChanges : List BlobChangeSequenceElement -> Bytes.Bytes -> Bytes.Bytes
applyBlobChanges changes blobBefore =
    (changes
        |> List.foldl
            (\change stateBefore ->
                case change of
                    ReuseBytes reuseBytes ->
                        { originalBlobRemainingBytes =
                            stateBefore.originalBlobRemainingBytes |> List.drop reuseBytes
                        , changedBlobBytes =
                            stateBefore.changedBlobBytes ++ (stateBefore.originalBlobRemainingBytes |> List.take reuseBytes)
                        }

                    RemoveBytes removeBytes ->
                        { stateBefore
                            | originalBlobRemainingBytes = stateBefore.originalBlobRemainingBytes |> List.drop removeBytes
                        }

                    AddBytes addBytes ->
                        { stateBefore
                            | changedBlobBytes = stateBefore.changedBlobBytes ++ Bytes.Extra.toByteValues addBytes
                        }
            )
            { originalBlobRemainingBytes = Bytes.Extra.toByteValues blobBefore
            , changedBlobBytes = []
            }
    ).changedBlobBytes
        |> Bytes.Extra.fromByteValues


diffChangesAggregateNoChangeSequences : List (Diff.Change (List a)) -> List (Diff.Change (List a))
diffChangesAggregateNoChangeSequences changes =
    let
        aggregateRecursiveReverted aggregate remaining =
            case remaining of
                [] ->
                    aggregate

                (Diff.NoChange firstNoChange) :: (Diff.NoChange secondNoChange) :: remainingTail ->
                    aggregateRecursiveReverted
                        aggregate
                        (Diff.NoChange (firstNoChange ++ secondNoChange) :: remainingTail)

                nextRemaining :: remainingTail ->
                    aggregateRecursiveReverted
                        (nextRemaining :: aggregate)
                        remainingTail
    in
    List.reverse (aggregateRecursiveReverted [] changes)


splitBlobValueAtLineBreaks : List Int -> List (List Int)
splitBlobValueAtLineBreaks blobValue =
    let
        isLineBreakCharacter char =
            char == 10 || char == 13

        breakRecursiveReverted broken remaining =
            case List.Extra.splitWhen isLineBreakCharacter remaining of
                Nothing ->
                    remaining :: broken

                Just ( nextElement, nextRemainingPlusHead ) ->
                    case nextRemainingPlusHead of
                        [] ->
                            remaining :: broken

                        nextElementTail :: nextRemaining ->
                            breakRecursiveReverted
                                ((nextElement ++ [ nextElementTail ]) :: broken)
                                nextRemaining
    in
    List.reverse (breakRecursiveReverted [] blobValue)
