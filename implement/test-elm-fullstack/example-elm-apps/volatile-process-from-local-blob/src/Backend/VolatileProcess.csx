#r "netstandard"
#r "System"
#r "System.Collections.Immutable"
#r "System.Net"
#r "System.Net.Http"
#r "System.Net.Primitives"
#r "System.Private.Uri"
#r "System.Linq"

/*
Use the `elm-fs  describe` command to get the hash of a file:

elm-fs  describe  ./assembly/assembly.dll
Loaded composition d0adb9c0632d35c7a982da279276879c7d49a99dd7c004277f78d34dd718e961 from './assembly/assembly.dll'.
Composition d0adb9c0632d35c7a982da279276879c7d49a99dd7c004277f78d34dd718e961 is a blob containing 4096 bytes
*/
#r "sha256:d0adb9c0632d35c7a982da279276879c7d49a99dd7c004277f78d34dd718e961"

//  https://www.nuget.org/api/v2/package/System.Text.Json/6.0.1
#r "sha256:35a2d352939db450f77063ec3efe47027fdfcf74a09078e4cb3c85cf697049e8"

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net.Http;


string InterfaceToHost_Request(string request)
{
    var jsonString = System.Text.Json.JsonSerializer.Serialize(new { field_name = 123 });

    /*
    Use names as found using ILSpy in d0adb9c0632d35c7a982da279276879c7d49a99dd7c004277f78d34dd718e961.
    */
    return assembly.Class.Function(request);
}

