
var elm_monarch = {
    // Set defaultToken to invalid to see what you do not tokenize yet
    // defaultToken: 'invalid',

    keywords: [
        'if', 'then', 'else',
        'case', 'of',
        'let', 'in',
        'module', 'import', 'exposing',
        'port',
        'as',
        '_'
    ],

    typeKeywords: [
        'type', 'alias'
    ],

    operators: [
        '=', '>', '<', '==', '<=', '>=', '/=',
        '&&', '||', '++', '+', '-', '*', '/', '//',
        '<<', '>>'
    ],

    // we include these common regular expressions
    symbols: /[=><!~?:&|+\-*\/\^%]+/,

    // C# style strings
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    // The main tokenizer for our languages
    tokenizer: {
        root: [
            [/^[a-z_$][\w$]*/, {
                cases: {
                    '@typeKeywords': 'type',
                    '@keywords': 'keyword',
                    '@default': 'function.name'
                }
            }
            ],

            [/[a-z_$][\w$]*/, {
                cases: {
                    'exposing': { token: 'keyword', next: '@after_keyword_exposing' },
                    '@typeKeywords': 'type',
                    '@keywords': 'keyword',
                    '@default': 'identifier'
                }
            }],


            [/[A-Z][\w\$]*/, 'type.identifier'],  // to show class names nicely

            // whitespace
            { include: '@whitespace' },

            // delimiters and operators
            [/[{}()\[\]]/, '@brackets'],
            [/[<>](?!@symbols)/, '@brackets'],
            [/@symbols/, {
                cases: {
                    '@operators': 'operator',
                    '@default': ''
                }
            }],

            // @ annotations.
            // As an example, we emit a debugging log message on these tokens.
            // Note: message are supressed during the first load -- change some lines to see them.
            [/@\s*[a-zA-Z_\$][\w\$]*/, { token: 'annotation', log: 'annotation token: $0' }],

            // numbers
            [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
            [/0[xX][0-9a-fA-F]+/, 'number.hex'],
            [/\d+/, 'number'],

            // delimiter: after number because of .\d floats
            [/[;,.]/, 'delimiter'],

            // strings
            [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
            [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

            // characters
            [/'[^\\']'/, 'string'],
            [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
            [/'/, 'string.invalid']
        ],

        comment: [
            [/{-/, 'comment', '@push'],    // nested comment
            [/-}/, 'comment', '@pop'],
            [/./, 'comment.content']
        ],

        after_keyword_exposing:
            [
                [/\(/, { token: 'delimiter', switchTo: 'exposing' }],
                [/$/, { token: 'whitespace', next: '@popall' }],
                [/^[a-z]/, { token: 'whitespace', next: '@popall' }],
                [/\)/, { token: 'delimiter', next: '@pop' }],
            ],

        exposing:
            [
                [/\)/, { token: 'delimiter', next: '@pop' }],
                [/$/, { token: 'whitespace', next: '@popall' }],
                [/^[a-z]/, { token: 'whitespace', next: '@popall' }],
                [/[A-Z_$][\w$]*/, { token: 'type' }],
                [/[a-z_$][\w$]*/, { token: 'function.name' }],
                [/\(/, { token: 'delimiter', next: '@push' }],
            ],

        string: [
            [/[^\\"]+/, 'string'],
            [/@escapes/, 'string.escape'],
            [/\\./, 'string.escape.invalid'],
            [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
        ],

        string_triple_quote: [
            [/"""/, 'string', '@pop'],
            [/./, 'string']
        ],

        whitespace: [
            [/[ \t\r\n]+/, 'white'],
            [/{-/, 'comment', '@comment'],
            [/--.*$/, 'comment'],
            [/"""/, 'string', '@string_triple_quote'],
        ],
    },
};

monaco.languages.register({ id: 'Elm' });

monaco.languages.setMonarchTokensProvider('Elm', elm_monarch);

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

monaco.editor.create(document.getElementById("container"), {
    theme: 'dark-plus',
    value: getCode(),
    language: 'Elm'
});

function getCode() {
    return `
module Test.TestSyntaxHighlighting exposing
    ( RecordAlias
    , Route(..)
    , functionWithLetBlockAndLocalFunction
    , reference
    )

import Bytes
import Json.Decode exposing (keyValuePairs)


type Route
    = ApiRoute
    | StaticFileRoute String


type alias RecordAlias typeVar =
    { primitiveField : Basics.Int
    , var : typeVar

    -- Single line comment
    }


functionWithLetBlockAndLocalFunction : Bytes.Bytes -> Int
functionWithLetBlockAndLocalFunction param =
    let
        {- Multi-line
           comment
            {- nested -}
        -}
        localFunction : Bytes.Bytes -> Int
        localFunction =
            Bytes.width
    in
    if (param |> localFunction) < 123 then
        "testing string literal" ++ """string literal

containing newline""" |> String.length

    else
        case "hello" of
            "specific case" ->
                1

            _ ->
                3


reference : Json.Decode.Decoder a -> Json.Decode.Decoder (List ( String, a ))
reference =
    keyValuePairs
`;
}
