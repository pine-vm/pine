// Example displaying diagnostics in an editor at https://microsoft.github.io/monaco-editor/playground.html

const marker0Range = new monaco.Range(2, 8, 2, 22);

const marker1Range = new monaco.Range(3, 2, 3, 11);

const editor = monaco.editor.create(document.getElementById("container"), {
    value: "function hello() {\n\talert('Hello world!');\n second line\n}",
    language: "custom",
    lightbulb: { enabled: true },
    theme: "vs-dark",
});

monaco.editor.setModelMarkers(editor.getModel(), "custom-diagnostics", [{
    message: "First custom diagnostic text",
    startLineNumber: marker0Range.startLineNumber,
    startColumn: marker0Range.startColumn,
    endLineNumber: marker0Range.endLineNumber,
    endColumn: marker0Range.endColumn,
    source: "custom",
    severity: monaco.MarkerSeverity.Info
},
{
    message: "Second custom diagnostic text",
    startLineNumber: marker1Range.startLineNumber,
    startColumn: marker1Range.startColumn,
    endLineNumber: marker1Range.endLineNumber,
    endColumn: marker1Range.endColumn,
    source: "custom",
    severity: monaco.MarkerSeverity.Error
}]);

