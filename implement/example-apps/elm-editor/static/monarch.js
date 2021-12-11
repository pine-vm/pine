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

window.elm_monarch = elm_monarch;

