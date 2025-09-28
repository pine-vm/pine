# Pine Roadmap

Here is a plan for what future releases of Pine will focus on.

## Runtime Efficiency - General

The runtime efficiency items are not related to the Pine language, but are implementation details of the execution engine that ships together with the primary distribution.

- [ ] Emit IR frames using specialized function interfaces from code analysis, to save allocations in call sites.
- [ ] In IR frames generated from static functions, link directly to call sites, to skip the general lookup of the target.
- [ ] Use the static program from code analysis to inline functions saving allocations of short-lived structures. (e.g. Pipeline sequences of `Result.map`)
- [ ] Use static program from code analysis to emit to more concrete form, like C# or WASM. 🏗️ Work in progress now...
- [x] For emitting to lower level forms, ensure tail call optimization works to avoid stack overflows (like it already does in the IR-interpreter).
- [ ] Introduce specialized representation for slice of blob value to make slicing cheaper.
- [ ] Introduce specialized representation for slice of list value to make slicing cheaper.
- [ ] Introduce specialized representation for concatenated blob value to make blob composition cheaper.
- [ ] Introduce specialized representation for concatenated list value to make list composition cheaper.
- [ ] Introduce specialized representation for `Int64` for cheaper integer arithmetic.
- [ ] Introduce specialized representation for `Int32` for cheaper integer arithmetic.

## App Developer Tools

- [ ] Tool for interactive inspection for Pine programs.
- [ ] Tool for interactive inspection with view based on Elm source code syntax.

## App Build System

- [ ] Support integrating contents of subdirectory of selected GitHub repository at build time. (e.g. app building website with documentation rendered to HTML)

## Portability

- [ ] Package a general VM (Eval method) in WASM.
- [ ] Package the language services in WASM.
- [ ] Package the language server in WASI.



