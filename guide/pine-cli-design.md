# Pine CLI Design

## Portable Compilation and Raw Pine Values

`pine  elm  compile` writes one selected declaration's compiled Pine value.
Compilation does not require `main`, an application interface, or a batch-compatible type;
functions taking or returning functions are valid selections.

```powershell
pine elm compile .\src\Tools.elm\canonicalize --output .\build\canonicalize.pine
pine value inspect .\build\canonicalize.pine
pine value apply .\build\canonicalize.pine --argument-value .\request.pine --output .\result.pine
pine value inspect .\src
pine value apply .\build\canonicalize.pine --argument-value .\src --output .\result.pine
```

Select a declaration explicitly with `.elm/name` (or `.elm\name`) or
`.elm#Lnumber`; selecting a module alone is an error. Source/project discovery
and `--input-directory` follow `pine run`, including manifest source
directories outside the project. Compilation includes the selected declaration's
required dependency closure, not every declaration in its module.

`pine  value  inspect  <path>` writes JSON containing `arity`, `isExpression`,
and `hash`. The hash is the canonical Pine hash-tree identity of the decoded
value, not a checksum of the file.
Recognized compiled wrappers and legacy function records supply their remaining
structural arity, accounting for collected arguments. Other encoded Pine
expressions take one environment argument; non-expression data has arity zero.

`pine  value  apply <file>  --output <file>` requires no source files, Elm parser,
compiler, or source-derived native accelerators. Repeat `--argument-value <file>`
once per argument in parameter order. There is no Elm literal parsing,
type-based input conversion, prompting, or named export selection. Partial
application produces another binary function value; additional arguments can
apply functions returned by functions. With no arguments a compiled
zero-parameter declaration is evaluated once, while an ordinary data value or
an unsupplied parameterized expression is returned unchanged. An attempt to
apply an argument to non-callable data fails.

When the inspect path or an `--argument-value` path is a directory, the command
loads its files and converts the tree using the standard encoding.
Paths in the tree are relative to the input directory, and the directory's own name is not included.
No source-file, build-directory, or hidden-file filtering is applied. As with the existing
loader, empty subdirectories are omitted; an empty input directory maps to an empty tree.

Each directory conversion is reported on stderr with the path, the number of files, and their aggregate content size in bytes.
Directory and binary-file arguments may be mixed and retain their command-line order.

All value files are standalone `PineTreeBinaryEncoding` encodings of one Pine
value, using blobs, lists, and internal references. Functions and their captured environments are ordinary data in this encoding.

Both `elm  compile` and `value  apply` require `--output`; parent directories are
created, and complete output atomically replaces an existing file only on
success. They reserve stdout for machine output; diagnostics go to stderr. Errors return a nonzero exit code.
Both accept `--timeout <duration>` (seconds by default) and Ctrl+C. Cancellation
is cooperative; the compiler's synchronous stages can finish before observing
it, but cancelled work must not publish an output file. Cancellation returns
130 and timeout returns 124.

For bootstrapping, invoke a pinned earlier release's `elm  compile` on the newer
compiler-service sources, then load/apply its binary output in the newer
runtime. The earlier compiler must support those sources and dependencies.
The in-process `PineValueApplication.Inspect` and `PineValueApplication.Apply` APIs provide the same source-independent inspection/application semantics.

## Configuring Color and Appearance

Commands should support configuring coloring and animations analog to the `cargo` tool:

(See <https://github.com/rust-lang/cargo/blob/d5cf55ea732582c1a513490732e62cf005f7dbd8/doc/book/src/reference/config.md?plain=1#L1454-L1529>)

### `[term]`

The `[term]` table controls terminal output and interaction.

#### `term.quiet`

* Type: boolean
* Default: false
* Environment: `PINE_TERM_QUIET`

Controls whether or not log messages are displayed by Pine.

Specifying the `--quiet` flag will override and force quiet output.
Specifying the `--verbose` flag will override and disable quiet output.

#### `term.verbose`

* Type: boolean
* Default: false
* Environment: `PINE_TERM_VERBOSE`

Controls whether or not extra detailed messages are displayed by Pine.

Specifying the `--quiet` flag will override and disable verbose output.
Specifying the `--verbose` flag will override and force verbose output.

#### `term.color`

* Type: string
* Default: `"auto"`
* Environment: `PINE_TERM_COLOR`

Controls whether or not colored output is used in the terminal. Possible values:

* `auto` (default): Automatically detect if color support is available on the
  terminal.
* `always`: Always display colors.
* `never`: Never display colors.

Can be overridden with the `--color` command-line option.

### Formatter status markers

The `dotnet format` and `elm format` subcommands use Unicode status markers
when the output supports Unicode. With legacy output encodings, they use
ASCII alternatives: `+` for success, `*` for files needing formatting, and
`x` for errors. This keeps summary borders and file lists aligned without
changing the terminal's encoding. The markers remain visible with `--color never`.
