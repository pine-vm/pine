module ElmSyntax.Path exposing (..)

{-| Range-free structural addressing of nodes in an Elm syntax tree.

A `Path` identifies a node by its position in the tree structure only. It
contains no source ranges and no source text, so it stays valid for the
abstract model (`ElmSyntax.Abstract.*`) as well as for the concrete model
(`ElmSyntax.Concrete.*`) that the abstract model was derived from.

The intended use is:

  - semantic analysis walks the abstract file and records paths;
  - a presentation layer resolves a path against the retained concrete file
    to obtain a source range on demand
    (see `ElmSyntax.Concrete.SourceLookup`).

Correspondence rules between the two models (see
`ElmSyntax.Abstract.ConvertFromConcrete`):

  - `Parenthesized` expressions and `ParenthesizedPattern` patterns exist only
    in the concrete model and are transparent for paths: a path never contains
    a step into them, the lookup skips them silently.
  - `SeparatedSyntaxList` (concrete) and `List` (abstract) keep the same order,
    so plain indices are stable for lists, tuples, constructors and exposing
    entries.
  - Record setters are sorted by field name in the abstract model while the
    concrete model keeps source order. Record fields are therefore addressed by
    field name plus an occurrence index (`StepRecordField`), which is stable in
    both models even for duplicate field names.
  - Literal spellings (`0x10` vs `16`, regular vs multiline strings) are
    normalized in the abstract model but do not affect the tree shape.
  - Documentation, comments and incomplete declarations exist only in the
    concrete model and are never addressed by a path step; they are reachable
    through `Selection` values instead.

-}


{-| Address of a node relative to the root of a file, outermost step first.
-}
type alias Path =
    List Step


{-| One step from a node to one of its children.

Not every step is valid for every node; resolving an inapplicable step yields
`Nothing` rather than a fabricated result.

-}
type Step
    = -- File -> module definition
      StepModuleDefinition
      -- File -> imports[i]
    | StepImport Int
      -- File -> declarations[i]
    | StepDeclaration Int
      -- Module/Import -> module name
    | StepModuleName
      -- Import -> alias module name
    | StepModuleAlias
      -- Module/Import -> explicit exposing entry i
    | StepExposingEntry Int
      -- Function/port declaration -> signature
    | StepSignature
      -- Function declaration -> implementation
    | StepImplementation
      -- Signature/type alias -> type annotation
    | StepTypeAnnotation
      -- Choice type declaration -> constructor i
    | StepConstructor Int
      -- Function implementation / lambda / constructor -> argument i
    | StepArgument Int
      -- Function implementation / lambda / let block / case branch -> body
    | StepBody
      -- Let expression -> declaration i
    | StepLetDeclaration Int
      -- Case expression -> branch i
    | StepCaseBranch Int
      -- Case branch / let destructuring -> pattern
    | StepPattern
      -- Positional child of an expression, pattern or type annotation
    | StepChild Int
      -- Record expression / record type -> field with the given name,
      -- disambiguated by occurrence index among fields of the same name
    | StepRecordField String Int


{-| Which part of the addressed node a source range is requested for.

Some semantic occurrences need a smaller range than the whole node, for
example the bare name of a qualified reference for a rename edit.

-}
type Selection
    = -- The complete source range of the addressed node
      SelectWhole
      -- The declared or referenced name token, without module qualifier
    | SelectName
      -- The module qualifier tokens of a qualified reference, if any
    | SelectQualifier
      -- Declaration range without its documentation comment, expanded to the
      -- start of the line
    | SelectDeclarationWithoutDocumentation
      -- Documentation comment of a declaration, if any
    | SelectDocumentation


{-| Appends a step to a path.
-}
appendStep : Path -> Step -> Path
appendStep path step =
    path ++ [ step ]


isPrefixOf : Path -> Path -> Bool
isPrefixOf outer inner =
    let
        innerSlice =
            List.take (List.length outer) inner
    in
    innerSlice == outer
