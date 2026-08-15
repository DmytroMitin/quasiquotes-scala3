# Versioning and stability

The project uses an experimental 0.x policy represented in published metadata
as `early-semver`.

- A breaking change to an intentionally public `core` or `frontend` surface
  increments the minor version, for example `0.1.x` to `0.2.0`.
- Patch releases within one minor line are expected to preserve source and
  binary use of that line. If compatibility is uncertain, use a new minor.
- No compatibility is promised between different 0.x minor lines.
- Documentation, tests, and additive implementation changes may ship in a
  patch only when they preserve the current minor-line contract.
- All compiler-line-specific `frontend` artifacts share one library version.

This policy does not make the current API stable. Public ADTs, functions,
extensions, diagnostics, and rendering are experimental. The maintained public
API inventory is a review/diff baseline, not a binary-compatibility guarantee.

The exact-shape inventory gate classifies no delta, additive-only review, and
removal/breaking risk. Additive overloads, givens, and extensions still require
human review before a patch release; they are not automatically source-safe.
See [public API shape compatibility review](API_COMPATIBILITY_REVIEW.md).

The current source line is `0.2.0-SNAPSHOT`. It deliberately replaces the
previous unusable `qq: Nothing` placeholder with the bounded Quotes-dependent
term extractor. That signature replacement is not source-, binary-, or
TASTy-compatible with the `0.1.x` line and is the reason for the minor change.
The bounded reflected `tqr` and `tqq` type syntax is additive within this
source line. Its extra sequence-shaped `tqr` overload is retained specifically
to preserve the prior varargs function's supported eta-expansion shape.
The bounded `DefinitionPattern.singleParameter` matcher is also additive within
this source line: it introduces new names and no overloads or replacements.
Its caller-`Quotes` reflected result remains experimental and is limited to the
documented exact single-parameter matcher grammar.

## Module-specific compatibility

- `core` uses the Scala 3 binary suffix `_3` and is built with the minimum
  supported compiler for publication. Its compiler-free boundary is tested,
  but compatibility across library minor lines is not promised.
- `frontend` uses the full Scala compiler version in its artifact name. A
  consumer must use the frontend artifact built for the same compiler line.
- `dottyInternal` is exact-compiler integration source and remains unpublished.
- The aggregate root and example modules remain unpublished.

Passing Scala 3.3.8, 3.8.4, 3.9.0-RC1, or nightly tests is evidence for those
tested revisions only. It is not a promise about future TASTy, reflection,
parser, or Dotty-internal compatibility.

A command-local candidate version used in a task-owned repository is review
evidence only. It does not change the retained source version, reserve a remote
version, authorize a tag, or establish that artifact bytes are reproducible.
