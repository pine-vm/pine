module MonacoHtml exposing (..)

{-| Functions for building the HTML document containing the Monaco Editor instance.
-}


monacoHtmlDocument : String
monacoHtmlDocument =
    monacoHtmlDocumentFromCdnUrl (monacoCdnURLs |> List.head |> Maybe.withDefault "Missing URL to CDN")


{-| <https://github.com/microsoft/monaco-editor/issues/583>
-}
monacoCdnURLs : List String
monacoCdnURLs =
    [ "https://cdn.jsdelivr.net/npm/monaco-editor@0.46.0/min"
    , "https://unpkg.com/monaco-editor@0.46.0/min"
    ]


{-| Combine from samples:

  - <https://github.com/microsoft/monaco-editor/blob/1396f98763b08e4b8dc3d9e16e23ceb67d8456e9/docs/integrate-amd.md>
  - <https://github.com/microsoft/monaco-editor/blob/1396f98763b08e4b8dc3d9e16e23ceb67d8456e9/docs/integrate-amd-cross.md>

-}
monacoHtmlDocumentFromCdnUrl : String -> String
monacoHtmlDocumentFromCdnUrl cdnUrlToMin =
    """
<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
    <script type="text/javascript" src="monarch.js"></script>
</head>
<body style="margin:0;height:93vh;">

<script type="text/javascript" src=\""""
        ++ cdnUrlToMin
        ++ """/vs/loader.js"></script>

<script>

    getCompletionItemsTimeoutMilliseconds = 2000;
    getHoverTimeoutMilliseconds = 1500;
    provideCompletionItemsEventFromElm = function(){};
    provideHoverEventFromElm = function(){};

    function getEditorModel() {
        if(typeof monaco != "object")
            return null;

        return monaco?.editor?.getModels()[0];
    }

    function monacoEditorSetContent(newValue, language, uri) {

        if(typeof monaco != "object")
            return;

        const editor =
            monaco.editor?.getEditors()[0];

        if(typeof editor != "object") {

            console.error("No editor found to set content.");

            return;
        }

        let monacoUri =
            monaco.Uri.parse(uri)

        var textModel =
            monaco.editor?.getModel(monacoUri);

        const reuseModel =
            textModel?.uri.toString() === uri

        /*
        console.log("previous model:");
        console.log(textModel);

        console.log("equal: " + (textModel?.uri.toString() === uri));
        */

        if(!reuseModel) {

            console.log("Creating new model for " + monacoUri);

            const modelsToDispose =
                monaco.editor?.getModels() ?? [];

            modelsToDispose.forEach(model => {

                console.log("Previous model uri: " + model.uri);

                model.dispose();
            });

            textModel =
                monaco.editor?.createModel(newValue, language, monacoUri);

            editor.setModel(textModel);
        }

        if(textModel == null) {
            return;
        }

        textModel.setValue(newValue);
        monaco.editor?.setModelLanguage(textModel, language);
    }

    function monacoEditorSetModelMarkers(markers) {

        if(typeof monaco != "object")
            return;

        monaco?.editor?.setModelMarkers(getEditorModel(), "", markers.map(monacoMarkerFromElmMonacoMarker));
    }

    function monacoEditorRevealPositionInCenter(position) {
        if (typeof theEditor === 'undefined')
            return;

        theEditor?.revealPositionInCenter(position);
    }

    function monacoMarkerFromElmMonacoMarker(elmMonacoMarker) {
        return {
            message : elmMonacoMarker.message,
            startLineNumber : elmMonacoMarker.startLineNumber,
            startColumn : elmMonacoMarker.startColumn,
            endLineNumber : elmMonacoMarker.endLineNumber,
            endColumn : elmMonacoMarker.endColumn,
            severity : monacoMarkerSeverityFromElmMonacoMarkerSeverity(elmMonacoMarker.severity),
        };
    }

    function monacoMarkerSeverityFromElmMonacoMarkerSeverity(elmMonacoMarkerSeverity) {
        if (typeof monaco === 'undefined')
            return -1;

        if (elmMonacoMarkerSeverity.ErrorSeverity != null)
            return monaco?.MarkerSeverity.Error;

        if (elmMonacoMarkerSeverity.WarningSeverity != null)
            return monaco?.MarkerSeverity.Warning;

        if (elmMonacoMarkerSeverity.InfoSeverity != null)
            return monaco?.MarkerSeverity.Info;

        if (elmMonacoMarkerSeverity.HintSeverity != null)
            return monaco?.MarkerSeverity.Hint;
    }

    function monacoCompletionItemFromElmMonacoCompletionItem(range, completionItem) {
        return {
                label: completionItem.label,
                kind: monacoCompletionItemKindFromElmCompletionItemKind(completionItem.kind),

                // https://github.com/microsoft/monaco-editor/issues/1074#issuecomment-423956977
                documentation: { value : completionItem.documentation },

                insertText: completionItem.insertText,
                range: range
            };
    }

    function monacoCompletionItemKindFromElmCompletionItemKind(completionItemKind) {
        if (typeof monaco === 'undefined')
            return -1;

        if (completionItemKind.ConstructorCompletionItemKind != null)
            return monaco?.languages.CompletionItemKind.Constructor;

        if (completionItemKind.EnumCompletionItemKind != null)
            return monaco?.languages.CompletionItemKind.Enum;

        if (completionItemKind.EnumMemberCompletionItemKind != null)
            return monaco?.languages.CompletionItemKind.EnumMember;

        if (completionItemKind.FunctionCompletionItemKind != null)
            return monaco?.languages.CompletionItemKind.Function;

        if (completionItemKind.ModuleCompletionItemKind != null)
            return monaco?.languages.CompletionItemKind.Module;

        if (completionItemKind.StructCompletionItemKind != null)
            return monaco?.languages.CompletionItemKind.Struct;

        console.error("Unexpected shape of completionItemKind: " + JSON.stringify(completionItemKind));
    }

    function dispatchMessage(message) {
        if(message.SetContent) {

            monacoEditorSetContent(
                message.SetContent[0].value,
                message.SetContent[0].language,
                message.SetContent[0].uri);
        }

        if(message.SetModelMarkers)
            monacoEditorSetModelMarkers(message.SetModelMarkers[0]);

        if(message.RevealPositionInCenter)
            monacoEditorRevealPositionInCenter(message.RevealPositionInCenter[0]);

        if(message.ProvideCompletionItemsEvent)
            provideCompletionItemsEventFromElm(message.ProvideCompletionItemsEvent[0]);

        if(message.ProvideHoverEvent)
            provideHoverEventFromElm(message.ProvideHoverEvent[0]);
    }

    function editorEventOnDidFocusEditorWidget() {
        parent?.messageFromMonacoFrame?.({ DidFocusEditorWidgetEvent: []});
    }

    function editorActionCloseEditor() {
        parent?.messageFromMonacoFrame?.({ EditorActionCloseEditorEvent:[]});
    }

    function editorActionFormatDocument() {
        parent?.messageFromMonacoFrame?.({ EditorActionFormatDocumentEvent: []});
    }

    function editorActionCompile() {
        parent?.messageFromMonacoFrame?.({ EditorActionCompileEvent: []});
    }

    function editorActionInspectSyntax() {
        parent?.messageFromMonacoFrame?.({ EditorActionInspectSyntaxEvent: []});
    }

    function editorProvideCompletionItemsFromRangeAndLeadingText(uri, range, textUntilPosition, cursorLineNumber) {

        return new Promise(function (resolve, reject) {

            const timeout =
                setTimeout(() => {
                    const message = "Did not get completion items from Elm within " + getCompletionItemsTimeoutMilliseconds + " milliseconds.";

                console.error(message);
                reject(message);
                return;
            }, getCompletionItemsTimeoutMilliseconds);

            provideCompletionItemsEventFromElm = function(completionItemsFromElm)
            {
                clearTimeout(timeout);

                const completionItemsForMonaco =
                    completionItemsFromElm.map(item => monacoCompletionItemFromElmMonacoCompletionItem(range, item))

                resolve({ suggestions: completionItemsForMonaco ?? [] });

                provideCompletionItemsEventFromElm = function(){};
            }

            parent?.messageFromMonacoFrame?.({
                RequestCompletionItemsEvent:
                    [
                    { uri: uri
                    , textUntilPosition: textUntilPosition
                    , cursorLineNumber: cursorLineNumber
                    }
                    ]});
        });
    }

    function editorProvideHoverFromPosition(uri, position, lineText, word) {

        return new Promise(function (resolve, reject) {

            const timeout =
                setTimeout(() => {
                    const message = "Did not get hover from Elm within " + getHoverTimeoutMilliseconds + " milliseconds.";

                console.error(message);
                reject(message);
            }, getHoverTimeoutMilliseconds);

            provideHoverEventFromElm = function(hoverFromElm)
            {
                clearTimeout(timeout);

                const contents =
                    hoverFromElm.map(content => ({ value: content }))

                resolve({ contents: contents ?? [] });

                provideHoverEventFromElm = function(){};
            }

            parent?.messageFromMonacoFrame?.({
                RequestHoverEvent:
                    [
                    { uri: uri
                    , positionLineNumber: position.lineNumber
                    , positionColumn: position.column
                    , lineText: lineText
                    , word: word.word
                    }
                    ]});
        });
    }


</script>

<script>
  monacoStorageSettingExpandSuggestionDocs = true;

  require.config({ paths: { 'vs': '"""
        ++ cdnUrlToMin
        ++ """/vs' }});

  // Before loading vs/editor/editor.main, define a global MonacoEnvironment that overwrites
  // the default worker url location (used when creating WebWorkers). The problem here is that
  // HTML5 does not allow cross-domain web workers, so we need to proxy the instantiation of
  // a web worker through a same-domain script
  window.MonacoEnvironment = {
    getWorkerUrl: function(workerId, label) {
      return `data:text/javascript;charset=utf-8,${encodeURIComponent(`
        self.MonacoEnvironment = {
          baseUrl: '"""
        ++ cdnUrlToMin
        ++ """/'
        };
        importScripts('"""
        ++ cdnUrlToMin
        ++ """/vs/base/worker/workerMain.js');`
      )}`;
    }
  };

    require(['vs/editor/editor.main'], function() {

        monaco.languages.register({
            id: 'Elm',
            aliases: ["elm"],
            extensions: [".elm"]
            });

        monaco.languages.setMonarchTokensProvider('Elm', window.elm_monarch);

        monaco.languages.registerCompletionItemProvider('Elm', {
            provideCompletionItems: function(model, position) {

                const uri =
                    model.uri.toString()

                console.log("position:");
                console.log(position);

                // find out if we are completing a property in the 'dependencies' object.
                const textUntilPosition =
                    model.getValueInRange({startLineNumber: 1, startColumn: 1, endLineNumber: position.lineNumber, endColumn: position.column})

                const word =
                    model.getWordUntilPosition(position)

                const range = {
                    startLineNumber: position.lineNumber,
                    endLineNumber: position.lineNumber,
                    startColumn: word.startColumn,
                    endColumn: word.endColumn
                };

                return editorProvideCompletionItemsFromRangeAndLeadingText(
                    uri,
                    range,
                    textUntilPosition,
                    position.lineNumber);
            },

            triggerCharacters: [".", " "]
        });

        monaco.editor.onDidCreateModel(function(model) {
            function forwardDidChangeContent() {
            
                const textModelValue = model.getValue();

                // console.log("onDidChangeContent:\\n" + textModelValue);

                const eventRecord = {
                    textModelValue : textModelValue,
                    uri : model.uri.toString()
                };

                parent?.messageFromMonacoFrame?.({
                    DidChangeContentEvent : [ eventRecord ]
                    });
            }

            var handle = null;
            model.onDidChangeContent(() => {
            // debounce
            clearTimeout(handle);
            handle = setTimeout(() => forwardDidChangeContent(), 400);
            });

            parent?.messageFromMonacoFrame?.({CompletedSetupEvent: []});
        });


        monaco.languages.registerHoverProvider('Elm', {
            provideHover: function (model, position) {

                const uri =
                    model.uri.toString()

                const textUntilPosition =
                    model.getValueInRange({startLineNumber: 1, startColumn: 1, endLineNumber: position.lineNumber, endColumn: position.column});

                const lineText =
                    model.getLineContent(position.lineNumber);

                const word =
                    model.getWordAtPosition(position);

                return editorProvideHoverFromPosition(uri, position, lineText, word);
            }
        });

        monaco.editor.defineTheme('dark-plus', {
            base: 'vs-dark',
            inherit: true,
            rules: [
                { token: 'keyword', foreground: '#C586C0' },
                { token: 'type', foreground: '#569CD6' },
                { token: 'function.name', foreground: '#DCDCAA' },
            ],
            colors: {},
        });

        customElements.define(
            "code-view-monaco",
            class CodeViewMonaco extends HTMLElement {
                _monacoEditor;
                /** @type HTMLElement */
                _editor;

                constructor() {
                    super();

                    /*
                    Setting up in the constructor would work when using shadow DOM, but since we integrate without shadow DOM, setup is done from connectedCallback.

                    this.setupMonaco();
                    */
                }

                connectedCallback() {

                    this.setupMonaco();
                }

                setupMonaco() {

                    const template = /** @type HTMLTemplateElement */ (
                        document.getElementById("editor-template")
                    );

                    const editorNode = template.content.cloneNode(true);

                    /*
                    At the moment, we avoid implementations placing Monaco in a shadow DOM because of this bug that breaks hover providers: <https://github.com/microsoft/monaco-editor/issues/3409>
                    Other features like markers and completion suggestions worked normally inside the shadow DOM, but the hover popups did not show up.

                    const shadowRoot = this.attachShadow({ mode: "open" });

                    // Copy over editor styles
                    const styles = document.querySelectorAll(
                        "link[rel='stylesheet'][data-name^='vs/']"
                    );

                    for (const style of styles) {
                        shadowRoot.appendChild(style.cloneNode(true));
                    }

                    shadowRoot.appendChild(editorNode);

                    this._editor = shadowRoot.querySelector("#monaco-container");
                    */

                    this.appendChild(editorNode);
                    this._editor = this.querySelector("#monaco-container");

                    this._monacoEditor = monaco.editor.create(this._editor, {
                        automaticLayout: true,
                        language: 'Elm',
                        value: "Initialization of editor is not complete yet",
                        scrollBeyondLastLine: false,
                        theme: "dark-plus",
                    }, {
                        // https://github.com/microsoft/monaco-editor/issues/2241#issuecomment-764694521
                        // https://stackoverflow.com/questions/54795603/always-show-the-show-more-section-in-monaco-editor/59040199#59040199
                        storageService: {
                            get(key) {
                                // console.log("storageService.get: " + key);
                            },
                            remove() { },
                            getBoolean(key) {
                                // console.log("storageService.getBoolean: " + key);

                                if (key === 'expandSuggestionDocs') {
                                    return monacoStorageSettingExpandSuggestionDocs;
                                }
                            },
                            getNumber(key) {
                                // console.log("storageService.getNumber: " + key);
                            },
                            store(key, value) {
                                // console.log("storageService.store: " + key);

                                if (key === 'expandSuggestionDocs')
                                    monacoStorageSettingExpandSuggestionDocs = value;
                            },
                            set(key) {
                                // console.log("storageService.set: " + key);
                            },
                            onWillSaveState() {},
                            onDidChangeStorage() {},
                            onDidChangeValue() {},
                        }
                    });

                    this._monacoEditor.onDidFocusEditorWidget(() => {

                        editorEventOnDidFocusEditorWidget();
                    });

                    this._monacoEditor.addAction({
                        id: 'close-editor-action',
                        label: 'Close Editor',
                        keybindings: [],
                        precondition: null,
                        keybindingContext: null,

                        contextMenuGroupId: 'z-other',
                        contextMenuOrder: 99,

                        run: function(ed) {
                            editorActionCloseEditor();
                            return null;
                        }
                    });

                    this._monacoEditor.addAction({
                        id: 'format-document-action',
                        label: 'Format Document',
                        keybindings: [
                            monaco.KeyMod.Shift | monaco.KeyMod.Alt | monaco.KeyCode.KEY_F
                        ],
                        precondition: null,
                        keybindingContext: null,

                        run: function(ed) {
                            editorActionFormatDocument();
                            return null;
                        }
                    });

                    this._monacoEditor.addAction({
                        id: 'compile-action',
                        label: 'Compile',
                        keybindings: [
                            monaco.KeyMod.Shift | monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter
                        ],
                        precondition: null,
                        keybindingContext: null,

                        run: function(ed) {
                            editorActionCompile();
                            return null;
                        }
                    });

                    this._monacoEditor.addAction({
                        id: 'inspect-syntax-action',
                        label: 'Inspect Syntax',
                        keybindings: [],
                        precondition: null,
                        keybindingContext: null,

                        run: function(ed) {
                            editorActionInspectSyntax();
                            return null;
                        }
                    });
                }
            }
        );
    });
</script>

<template id="editor-template">
\t<div
\t\tid="monaco-container"
\t\tstyle="overflow: hidden; width: 100%; height: 100%; position: absolute"
\t></div>
</template>

<code-view-monaco></code-view-monaco>

</body>
</html>
"""
