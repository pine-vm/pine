module ElmMake.Runtime exposing (functions, html, script)

import ElmMake.Model exposing (Flavor(..))


{-| Calling-convention helpers compatible with Elm compiler 0.19.1.
The layout follows Generate.JavaScript.Functions in elm/compiler (BSD-3-Clause).
-}
functions : String
functions =
    """

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

"""


script : Flavor -> String -> String -> String -> String
script flavor kernels declarations exports =
    "(function(scope){\n'use strict';"
        ++ functions
        ++ (case flavor of
                Optimize ->
                    ""

                _ ->
                    "console.warn('Compiled in "
                        ++ (if flavor == Debug then
                                "DEBUG"

                            else
                                "DEV"
                           )
                        ++ " mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');"
           )
        ++ kernels
        ++ declarations
        ++ "_Platform_export("
        ++ exports
        ++ ");}(this));"


html : String -> String -> String
html moduleName javascript =
    "<!DOCTYPE HTML>\n<html>\n<head>\n  <meta charset=\"UTF-8\">\n  <title>"
        ++ moduleName
        ++ "</title>\n  <style>body { padding: 0; margin: 0; }</style>\n</head>\n\n<body>\n\n<pre id=\"elm\"></pre>\n\n<script>\ntry {\n"
        ++ javascript
        ++ "\n\n  var app = Elm."
        ++ moduleName
        ++ ".init({ node: document.getElementById(\"elm\") });\n}\ncatch (e)\n{\n  // display initialization errors (e.g. bad flags, infinite recursion)\n  var header = document.createElement(\"h1\");\n  header.style.fontFamily = \"monospace\";\n  header.innerText = \"Initialization Error\";\n  var pre = document.getElementById(\"elm\");\n  document.body.insertBefore(header, pre);\n  pre.innerText = e;\n  throw e;\n}\n</script>\n\n</body>\n</html>"
