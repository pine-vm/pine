#!/usr/bin/env bash

set -e

dotnet build "$(pwd)/../../../pine/pine.csproj" --no-restore

export PATH="$(pwd)/../../../pine/bin/Debug/net10.0:$PATH"
export CODE_TESTS_PATH="$(pwd)/client/out/test"
export CODE_TESTS_WORKSPACE="$(pwd)/client/testFixture"

node "$(pwd)/client/out/test/runTest"