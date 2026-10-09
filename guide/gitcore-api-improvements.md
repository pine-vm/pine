# GitCore API and functionality ideas

These are brainstorming proposals, not commitments or implemented features. The motivating case is
loading an Elm project from a pinned GitHub tree URL before compiling and listing its tests.

## Starting point

GitCore already supports managed Smart HTTP loading, including fetching just a selected subdirectory.
`LoadTreeContentsFromUrlAsync` accepts GitHub/GitLab tree URLs and an optional `HttpClient`.
`LoadSubdirectoryContentsFromGitUrlAsync` accepts an explicit repository URL, commit and path, plus
blob-cache lookup and loaded-blob callbacks.

The installed package's README and XML API documentation distinguish this remote API from
`LocalGitRepository`: local traversal already offers selection callbacks, cancellation, streamed file
content, bounded caches, object-size limits and typed error contexts. The ideas below aim to bring those
controls together for remote consumers rather than assume they are missing throughout GitCore.

API observations are based on the package README/XML documentation and
[the matching remote loader source](https://github.com/Viir/GitCore/blob/d6546de23ec2df359ce797e1815ac101b75b9d95/implement/GitCore/LoadFromUrl.cs).
The browser-URL helper already delegates to selective subdirectory loading; it is not necessary to
replace that helper merely to avoid downloading unrelated file contents.

## Source identity and URL resolution

- Separate parsing, ref resolution and content loading. Return a source descriptor containing the
  repository URL, requested ref, resolved commit, selected path and canonical pinned URL.
- Let callers supply an explicit ref and path instead of relying on the ambiguous boundary between a
  slash-containing branch name and a subdirectory in a browser URL.
- Specify behavior for repository roots, tree versus blob URLs, percent-encoded components, GitLab
  groups, annotated tags, queries and fragments. Return actionable parsing errors before networking.
- Preserve the resolved identity in load results so consumers can produce reproducible commands and
  reports without performing another ref lookup.

## Progress and cancellation

- Add a common remote-load options object with a `CancellationToken` and structured progress callback.
  The current high-level remote signatures do not expose those controls.
- Report phases such as ref resolution, tree discovery, cache lookup, object download, pack decoding
  and completion. Include elapsed time, transferred bytes and available object/file counts without
  inventing a percentage when the total is unknown.
- Propagate cancellation through HTTP, pack processing and tree traversal. Cancellation should stop
  underlying work, not merely stop awaiting its result.
- Keep presentation outside GitCore: CLI consumers render text, while other consumers can record
  structured events. Avoid credentials and unnecessary private paths in events.

## Cache and offline policy

- Expose existing blob-cache callbacks through the browser-URL convenience API, with explicit
  hit/miss reporting.
- Consider a shared object-store interface for blobs, trees and commits, so repeated subdirectory
  loads reuse both discovery data and content. Verify fetched content before admitting it to a cache.
- Distinguish immutable commit content from mutable branch resolution; make refresh policy explicit.
- Provide a strict cache-only mode that never contacts the remote, reports the missing objects or ref
  metadata, and differs from a genuine missing-path result. Caching blobs alone cannot guarantee an
  offline load if commit/tree/ref data is absent.
- Define concurrent writer behavior, atomic publication, eviction and corrupted-entry recovery rather
  than requiring every caller to solve them.

## Incremental project loading

- Offer remote equivalents of local subtree/file selection and streamed content APIs. Avoid requiring
  a dictionary containing every file when a consumer needs only a manifest and selected sources.
- Support a bounded set of paths from one resolved commit. An Elm manifest can reference `../src`;
  consumers should be able to request that sibling tree without downloading unrelated projects.
- Expose file modes and object identities so consumers can choose explicit policies for symlinks,
  executable files and submodules instead of losing that information during byte-only loading.
- Add remote limits for transferred bytes, materialized object sizes, files and traversal depth.
  Keep transport concurrency bounded and configurable.

## Errors and transport policy

- Bring the local API's typed error/context approach to remote loads: distinguish URL syntax, missing
  ref, missing path, unsupported server capabilities, HTTP/authentication failures, corrupt objects,
  cache failures, resource limits and cancellation.
- Preserve useful causal diagnostics without leaking authentication material. Let the CLI map ordinary
  load failures to concise messages rather than depend on matching exception text.
- Retain injectable `HttpClient` support and make redirect/allowed-host policy explicit. Keep
  authentication out of URLs, logs and cache keys.
- Document retry policy and server capability fallback. A fallback that fetches a larger tree should
  obey the same resource limits and be visible to callers.

## Suggested validation

- Test pinned commits, branch/tag resolution, slash-containing refs, encoded paths and GitLab groups.
- Assert progress is emitted before slow transport work and cancellation actually stops that work.
- Cover warm-cache and strict-offline loads, partial/corrupt caches, and concurrent requests.
- Use deterministic transport fixtures for protocol/error cases and a small pinned public project for
  end-to-end verification; keep network-required tests distinguishable from offline tests.
- Verify path traversal, platform-specific filename collisions and symlink policies in any filesystem
  materialization layer. Git tree paths must not be trusted as native destination paths.

An initial API iteration could focus on source descriptors, progress/cancellation and cache policy.
Incremental remote traversal and multi-path project selection could then build on the same contracts.
