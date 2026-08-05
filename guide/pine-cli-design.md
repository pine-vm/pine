# Pine CLI Design

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

