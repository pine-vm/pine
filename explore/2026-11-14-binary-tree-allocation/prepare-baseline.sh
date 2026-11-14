#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p obj/baseline

python - <<'PY'
from pathlib import Path
import subprocess

baseline = "00b5a1ad5c23be8222d31578ae95eaa9a8fb9218"
files = {
    "Pine.cs": "implement/Pine.Core/Encodings/ValueEncodingBinaryDeterministic.cs",
    "Pine.Errors.cs": "implement/Pine.Core/Encodings/ValueEncodingBinaryDeterministic.Errors.cs",
    "Syntax.cs": "implement/Pine.Core/Elm/Syntax/Abstract/ElmSyntaxAbstractBinary.cs",
    "Syntax.Decoder.cs": "implement/Pine.Core/Elm/Syntax/Abstract/ElmSyntaxAbstractBinary.Decoder.cs",
    "BinaryEncoding.cs": "implement/Pine.Core/Encodings/BinaryEncoding.cs",
}
for name, path in files.items():
    source = subprocess.check_output(["git", "show", f"{baseline}:{path}"], text=True)
    if name.startswith("Pine"):
        source = source.replace("namespace Pine.Core.Encodings;", "namespace AllocationBaseline;")
    elif name.startswith("Syntax"):
        source = "using Pine.Core.Elm.Syntax.Abstract;\nusing Expression = Pine.Core.Elm.Syntax.Abstract.Expression;\n" + source.replace(
            "namespace Pine.Core.Elm.Syntax.Abstract;", "namespace AllocationBaseline;")
    Path("obj/baseline", name).write_text(source)
PY
