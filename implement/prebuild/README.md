# prebuild

The `prebuild.csproj` program builds artifacts to be embedded into the Pine assembly on build, and therefore, need to be built before building `Pine.Core.csproj`

Prebuild writes the reused Pine value bundle, including keys for the handwritten
`PineValueInProcess` leaves. It no longer generates or loads C# compiler bundles.
