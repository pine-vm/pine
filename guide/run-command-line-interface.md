# Run and Apply Command Line Interface

`pine run` selects an execution mode from the selected Elm declaration's type:
run a command-line application, or apply a function to one batch input.
`pine apply` provides the same batch functionality but rejects interactive
application declarations. Use `apply` when you intend to process one input and
want to ensure that selecting a different declaration cannot start an application.

```console
pine run <entry-point> [options]
pine apply <entry-point> [options]

Arguments:
  <entry-point>  Elm declaration (.elm/name or .elm#Lnumber), or a module for interactive selection.
                 Accepts local paths and HTTPS GitHub/GitLab file URLs. No default declaration names.

Source selection (both commands):
  --input-directory <directory>  Source directory for a relative local entry point.

Batch input/output options:
  --input <expression>           Elm literal, list, tuple, record, or choice constructor.
  --input-file <file>            Local input file or GitHub/GitLab file URL.
  --output-file <file>           Write only to this local file; overwrite without prompting.
  --transform-file <file>        Read and replace the same local file.

Options for both commands:
  --color <Always|Auto|Never>     Diagnostic color; overrides PINE_TERM_COLOR.
  --report-durations            Report compilation and execution durations on stderr.
  --timeout <duration>          Cooperative whole-command timeout; default unit is seconds.
  -?, -h, --help                Show help and usage information.
```

## Execution modes

| Mode | Selected declaration's type | Input and output |
| --- | --- | --- |
| Application | A record with `init` and `subscriptions` functions, implementing Pine's command-line application interface. | The application receives stdin, emits stdout/stderr, and chooses its exit status. |
| Batch | A function taking one input and returning a non-function value. | Pine obtains one input from an option or a prompt, applies the function once, and writes the result to stdout or a file. |

| Command | Supported modes |
| --- | --- |
| `pine run` | Application or batch, selected from the declaration's type. |
| `pine apply` | Batch only. Application declarations are rejected before they are evaluated or started; the error shows the declaration's signature and suggests `pine run`. |

There is no mode-selection flag. With `run`, the declaration's **type**, not its
name or path format, determines the mode. For example, a `runRoot : String -> Int`
declaration uses batch mode with either command. Choosing `apply` constrains the
same implementation to batch mode; it does not change batch parsing, prompting,
evaluation, input/output options, diagnostics, limits, or caching.

Unsupported types produce an error with the selected declaration's signature.
Type inference is limited; add explicit annotations when inference cannot
determine the type. `apply` does not replace `apply-function`, which applies a
function to an application's persisted database state.

Whenever a supported mode is selected, Pine explains why on stderr and shows the
selected declaration's name and inferred type in Elm source syntax (`name : Type`).
This happens on successful runs too, including when a module or line selector did
not explicitly name the declaration. An undetermined part of a type is shown as `?`.

## Selecting a declaration

```console
pine  apply  src/Tools.elm/characterCount --input "hello world"
pine  apply  https://github.com/pine-vm/pine/blob/339b0e85c950501911935af9434be885e6c006e8/implement/Pine.Core/Elm/elm-in-elm/elm-kernel-modules/Url.elm/fromString --input "https://example.com"
pine  apply  https://github.com/pine-vm/pine/blob/339b0e85c950501911935af9434be885e6c006e8/implement/Pine.Core/Elm/elm-in-elm/elm-kernel-modules/Url.elm#L50 --input "https://example.com"
```

A line selector can point anywhere inside a function's documentation, signature, or body.
Explicit selection also permits unexposed functions. Local paths accept `/` or `\`.

Both commands require an explicit choice of declaration. There is **no default-name
search**: `main`, `runRoot`, and `webServiceMain` are ordinary declaration names,
not implicit entry points. Even a module with only one declaration requires a
selection. Use `.elm/name` or `.elm#Lnumber` in scripts and pipelines.

When stdin is an interactive terminal, you can supply only the module:

```console
pine apply src/Tools.elm
```

On a capable terminal, a searchable, paginated picker lists declarations in
alphabetical order. Use Up/Down and Enter, or type to filter the list. The first
menu entry opens a plain-text numbered list where you can enter an exact,
case-sensitive declaration name or its displayed number. Terminals without cursor
control use this plain-text selection automatically. Disabling color does not
disable arrow navigation on a capable terminal.

The picker shows source type annotations where available and marks declarations
without annotations as not yet inferred. These annotations are not verified menu
classifications: only the selected declaration is compiled and its inferred type
determines whether execution is supported. Candidates are never evaluated merely
to populate the list. `apply` still rejects an application selected through the
picker; it does not fall back to a different declaration.

Selection UI goes to stderr, leaving stdout for results. The selected name remains
in the terminal output after the picker closes. Ctrl+C and `--timeout` also apply
while choosing a declaration.

When stdin is redirected, a missing selection is an error listing the available
declarations; neither command reads the redirected input to make a choice. An
invalid explicit name or line is also an error, not a request to choose again.
This keeps declaration selection separate from batch data or an application's
stdin. After an interactive declaration choice, a batch input prompt may follow
if no batch input option was supplied.

### Sources and project discovery

Pine discovers a governing `elm.json` and resolves its source directories and dependencies.
For standalone modules, it includes the ancestor sources needed by imports, with bundled
core modules available. `--input-directory` explicitly selects the local source directory
instead; the entry-point path is relative to it. Other local file options remain relative
to the current working directory.

Remote discovery uses shallow commit/tree metadata, the selected module, and ancestor
manifests before downloading the selected sources. Unrelated repository contents are
not loaded. Git blobs are verified and cached by content hash.

Remote references require an explicit branch or full 40-character commit SHA.
Repository-only URLs, default-branch guessing, and branch names containing slashes are
not supported. Prefer commit-pinned URLs for reproducible commands.

## Application mode (`run` only)

```console
pine run src/App.elm/runRoot
pine run src/App.elm/customApplication
```

A command-line application exposes `init` and `subscriptions` functions through a
record. Pine initializes the application with the command line and environment
variables, then forwards stdin to its subscriptions. The application can emit
stdout and stderr and request an exit status.

An application must be selected by name, by line, or by an explicit choice in the
interactive declaration picker. A declaration named `runRoot` is not selected
automatically. None of these selectors changes its input/output behavior.
Redirected stdin is forwarded to the application rather than interpreted as an
Elm batch input, and there is no batch input prompt.

`--input`, `--input-file`, `--output-file`, and `--transform-file` are batch-only
options and are rejected in application mode. Source selection, color, duration
reporting, and timeout options apply to either mode.

Web-service hosting is a separate command: use `pine run-server`, not `pine run`.

## Batch mode (`run` and `apply`)

Both commands share the batch behavior described below. The selected function
must take one input and return a non-function value:

```elm
characterCount : String -> Int
characterCount text =
    String.length text
```

```console
pine apply src/Tools.elm/characterCount --input "hello world"
pine run src/Tools.elm/characterCount --input "hello world"
```

Either command produces the same batch result on stdout:

```elm
11
```

A list, tuple, or record counts as one input. Curried functions such as
`String -> String -> String` are not supported.

### Supplying input

Supply one input using one of these methods:

| Method | Behavior |
| --- | --- |
| `--input <expression>` | Parse the option's value as an Elm input, with the string handling described below. |
| `--input-file <file>` | Read a local file or a GitHub/GitLab file URL; the parameter must accept `String` or `Bytes`. |
| `--transform-file <file>` | Read a local file and replace it with the function's result. |
| No input option | Show the options hint and prompt for one line on stdin. Parse and validate it exactly like `--input`. |

The three input options are mutually exclusive.
`--transform-file` cannot be combined with any other batch input/output option.
Explicit input options skip the prompt and do not read stdin.

Without an input option, run the same function interactively:

```console
pine apply src/Tools.elm/characterCount
```

After explaining the selected mode and showing the declaration's signature, Pine
lists `--input`, `--input-file`, and `--transform-file` as alternatives to entering
input at the prompt:

```console
Input> hello world
11
```

The prompt and hint go to stderr; only the result goes to stdout. A blank line is
a valid empty `String`, not end-of-input. Redirected stdin also supplies one line
when no input option is given. End-of-input without a line is an error, as is an
invalid input expression or a value that does not match the parameter type.

Both `--input` and prompted input accept literals, lists, tuples, records, and choice constructors,
not arbitrary computations or references to functions. Constructor matching is structural:
the constructor name, argument count, and argument types must match the parameter type;
the module qualifier does not establish nominal identity.

For a `String` parameter, an argument not beginning with a double quote (after leading
whitespace) is used verbatim. A leading double quote selects Elm string-literal parsing,
including escapes; malformed literals are errors. Shell quoting is processed before
Pine receives an `--input` argument. For example, in PowerShell:

```powershell
pine apply src/Tools.elm/characterCount --input 'hello world'
pine apply src/Tools.elm/characterCount --input '"hello\nworld"'
```

At the prompt, enter the value directly, without shell quoting. For example,
`hello world` is an unquoted string, `"hello\nworld"` is an Elm string literal,
and `["hello", "world"]` is a list of strings for a `List String` parameter.

### File input and output

```console
pine  apply  src/Tools.elm/normalize  --input-file input.txt  --output-file output.txt
pine  apply  src/Tools.elm/normalize  --transform-file document.txt
```

These options control batch input and results, not an application's stdin/stdout.
`--output-file` can be combined with `--input`, `--input-file`, or prompted input.

File input supports `String` and `Bytes` parameters. Bytes pass through unchanged.
For strings, a UTF-8, UTF-16, or UTF-32 BOM selects encoding and byte order and is
removed from the input string. The longest matching BOM wins; without a BOM,
input is strict UTF-8. Invalid byte sequences are errors.

File output writes `Bytes` verbatim and `String` as encoded text without Elm quotes.
Other Elm values are rendered as Elm expressions before encoding. If file input was
decoded to a string, text output preserves its encoding, byte order, and BOM presence.
Otherwise, text output uses UTF-8 without a BOM. No newline is appended to file output.

Destinations must be local files with an existing parent directory. Reading,
evaluation, conversion, and a temporary-file write complete before replacement.
Failures or cancellation before replacement leave an existing destination unchanged.

### Batch results

Without a file destination, batch results are rendered as Elm expressions followed
by a newline. Strings retain quotes and escapes. Bytes are the display exception:
three bytes `00 01 ff` render as `Bytes 3 0x0001ff`, including when nested in another value.

With `--output-file` or `--transform-file`, the result is written only to the file.
Successful batch execution exits with status `0`. Elm values such as `Err "message"`
are ordinary successful results, not command failures.

## Shared diagnostics and limits

Pine's mode explanation, selected declaration signature, source-loading and cache
messages, timings, errors, and work counters go to stderr with both commands. Batch
input hints and prompts also go to stderr. Application output still follows the
application's own stdout/stderr commands.

`--color` affects Pine's diagnostics and type signatures, not batch result data or
application output. Cursor-control capability is detected independently of color.
`--report-durations` reports `compilation` (including declaration selection, loading, and dependency
resolution) and `execution`. Execution includes batch input/output conversion and
time waiting at the input prompt, or the application's runtime until it exits.

`--timeout 30` or `--timeout 500ms` sets a cooperative wall-clock deadline for the
whole command, including remote loading, declaration selection, and waiting at a batch input prompt.
Synchronous loading, compilation, and native
operations may finish before reaching their next cancellation checkpoint. No deadline
is imposed when the option is omitted.

With either command, Pine reports errors with exit status `1`, timeouts with `124`, and
cancellation with `130`. Otherwise, batch execution exits with `0`, while an
application started with `run` chooses its exit status. If an application reaches stdin end-of-input
without requesting an exit status, Pine exits with `0`.

## Caching

Branch resolution is shared by all Pine commands that load Git files or directories.
Each remote repository/branch is pinned once per invocation, including input-file
references.
Every branch reference logs its repository, branch name, and resolved full commit
ID on stderr, including when the mapping comes from memory or disk cache.
Full commit IDs bypass branch resolution.

Compiled Pine code and type information are cached on disk by source contents,
resolved dependencies, selected declaration, and compiler/runtime version, not by
Git commit or runtime input. Identical source trees can reuse compilation across commits.
