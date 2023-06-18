module MonacoHtml exposing (..)

{-| Functions for building the HTML document containing the Monaco Editor instance.
-}


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

<div id="container" style="height:100%;width:100%;"></div>

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

    function monacoEditorSetValue(newValue) {
        getEditorModel()?.setValue(newValue);
    }

    function monacoEditorSetModelMarkers(markers) {
        if (typeof monaco === 'undefined')
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
        if(message.SetValue)
            monacoEditorSetValue(message.SetValue[0]);

        if(message.SetModelMarkers)
            monacoEditorSetModelMarkers(message.SetModelMarkers[0]);

        if(message.RevealPositionInCenter)
            monacoEditorRevealPositionInCenter(message.RevealPositionInCenter[0]);

        if(message.ProvideCompletionItemsEvent)
            provideCompletionItemsEventFromElm(message.ProvideCompletionItemsEvent[0]);

        if(message.ProvideHoverEvent)
            provideHoverEventFromElm(message.ProvideHoverEvent[0]);
    }

    function tryCompleteSetup() {
        var editorModel = getEditorModel();

        if(editorModel == null) {
            setTimeout(tryCompleteSetup, 500);
        }
        else {
            editorModel.onDidChangeContent(function() {
                var content = getEditorModel().getValue();

                // console.log("onDidChangeContent:\\n" + content);

                parent?.messageFromMonacoFrame?.({"DidChangeContentEvent":[content]});
            });

            parent?.messageFromMonacoFrame?.({"CompletedSetupEvent":[]});
        }
    }

    function editorEventOnDidFocusEditorWidget() {
        parent?.messageFromMonacoFrame?.({"DidFocusEditorWidgetEvent":[]});
    }

    function editorActionCloseEditor() {
        parent?.messageFromMonacoFrame?.({"EditorActionCloseEditorEvent":[]});
    }

    function editorActionFormatDocument() {
        parent?.messageFromMonacoFrame?.({"EditorActionFormatDocumentEvent":[]});
    }

    function editorActionCompile() {
        parent?.messageFromMonacoFrame?.({"EditorActionCompileEvent":[]});
    }

    function editorActionInspectSyntax() {
        parent?.messageFromMonacoFrame?.({"EditorActionInspectSyntaxEvent":[]});
    }

    function editorProvideCompletionItemsFromRangeAndLeadingText(range, textUntilPosition, cursorLineNumber) {

        return new Promise(function (resolve, reject) {

            var timeout =
                setTimeout(() => {
                    var message = "Did not get completion items from Elm within " + getCompletionItemsTimeoutMilliseconds + " milliseconds.";

                console.error(message);
                reject(message);
                return;
            }, getCompletionItemsTimeoutMilliseconds);

            provideCompletionItemsEventFromElm = function(completionItemsFromElm)
            {
                clearTimeout(timeout);

                var completionItemsForMonaco =
                    completionItemsFromElm.map(item => monacoCompletionItemFromElmMonacoCompletionItem(range, item));

                resolve({ suggestions: completionItemsForMonaco ?? [] });

                provideCompletionItemsEventFromElm = function(){};
            }

            parent?.messageFromMonacoFrame?.({
                "RequestCompletionItemsEvent":
                    [{"textUntilPosition":textUntilPosition,"cursorLineNumber":cursorLineNumber}]});
        });
    }

    function editorProvideHoverFromPosition(position, lineText, word) {

        return new Promise(function (resolve, reject) {

            var timeout =
                setTimeout(() => {
                    var message = "Did not get hover from Elm within " + getHoverTimeoutMilliseconds + " milliseconds.";

                console.error(message);
                reject(message);
            }, getHoverTimeoutMilliseconds);

            provideHoverEventFromElm = function(hoverFromElm)
            {
                clearTimeout(timeout);

                var contents = hoverFromElm.map(content => ({ value: content }));

                resolve({ contents: contents ?? [] });

                provideHoverEventFromElm = function(){};
            }

            parent?.messageFromMonacoFrame?.({
                "RequestHoverEvent":
                    [{"positionLineNumber":position.lineNumber,
                    "positionColumn":position.column,
                    "lineText": lineText,
                    "word": word.word}]});
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

        monaco.languages.register({ id: 'Elm' });

        monaco.languages.setMonarchTokensProvider('Elm', window.elm_monarch);

        monaco.languages.registerCompletionItemProvider('Elm', {
            provideCompletionItems: function(model, position) {
                // find out if we are completing a property in the 'dependencies' object.
                var textUntilPosition = model.getValueInRange({startLineNumber: 1, startColumn: 1, endLineNumber: position.lineNumber, endColumn: position.column});

                var word = model.getWordUntilPosition(position);
                var range = {
                    startLineNumber: position.lineNumber,
                    endLineNumber: position.lineNumber,
                    startColumn: word.startColumn,
                    endColumn: word.endColumn
                };

                return editorProvideCompletionItemsFromRangeAndLeadingText(range, textUntilPosition, position.lineNumber);
            },

            triggerCharacters: ["."," "]
        });

        monaco.languages.registerHoverProvider('Elm', {
            provideHover: function (model, position) {
                var textUntilPosition = model.getValueInRange({startLineNumber: 1, startColumn: 1, endLineNumber: position.lineNumber, endColumn: position.column});

                var lineText = model.getLineContent(position.lineNumber);
                var word = model.getWordAtPosition(position);

                return editorProvideHoverFromPosition(position, lineText, word);
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

        var editor = monaco.editor.create(document.getElementById('container'), {
            value: "Initialization of editor is not complete yet",
            language: 'Elm',
            automaticLayout: true,
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

        editor.onDidFocusEditorWidget(() => {

            editorEventOnDidFocusEditorWidget();
        });

        editor.addAction({
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

        editor.addAction({
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

        editor.addAction({
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

        editor.addAction({
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

        window.theEditor = editor;

        tryCompleteSetup();
    });
</script>

</body>
</html>
"""
