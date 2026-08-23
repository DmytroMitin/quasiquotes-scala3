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

The released line is the immutable `0.2.0` artifact set. The current source
line is the unpublished `0.3.0-SNAPSHOT` development version. Phase 116
replaces the public P1 block-prefix shape with a truthful statement ADT so a
local value definition is represented as a statement rather than a term. That
public shape replacement is intentionally breaking and therefore requires a
new early-semver minor; no released `0.2.0` artifact is modified. The earlier
`0.2.0` line deliberately replaced the unusable `qq: Nothing` placeholder with
the bounded Quotes-dependent term extractor relative to `0.1.x`.

The bounded reflected `tqr` and `tqq` type syntax was additive within the
released `0.2.0` line. Its extra sequence-shaped `tqr` overload is retained
specifically to preserve the prior varargs function's supported eta-expansion shape.
The bounded `DefinitionPattern.singleParameter` matcher is also additive within
this source line: it introduces new names and no overloads or replacements.
Its caller-`Quotes` reflected result remains experimental and is limited to the
documented exact single-parameter matcher grammar.
The bounded `DefinitionPattern.dqq` extension and
`SingleParameterDefinitionPattern.unapply` protocol are two further additive
members on those existing owners. They delegate to the same matcher grammar,
capture only the original complete RHS `Term`, and add no overload or implicit
replacement of an existing public name.

## Module-specific compatibility

- `core` uses the Scala 3 binary suffix `_3` and is built with the minimum
  supported compiler for publication. Its compiler-free boundary is tested,
  but compatibility across library minor lines is not promised.
- `frontend` uses the full Scala compiler version in its artifact name. A
  consumer must use the frontend artifact built for the same compiler line.
- `neutralScalameta` uses the Scala 3 binary suffix `_3` but remains
  unpublished. Its direct Scalameta AST boundary, admitted projection API, and
  any consumer-local `n*` aliases are experimental and carry no compatibility
  promise.
- `dottyInternal` is exact-compiler integration source and remains unpublished.
  Its narrow public peer bridge is an experimental JVM-access seam, not a
  stable general raw-tree API or a cross-compiler compatibility promise.
- The aggregate root and example modules remain unpublished.

Passing Scala 3.3.8, 3.8.4, 3.9.0-RC1, or nightly tests is evidence for those
tested revisions only. It is not a promise about future TASTy, reflection,
parser, or Dotty-internal compatibility.

Local signed staging and coordinate-only consumption are review evidence only.
The development snapshot does not authorize remote snapshot publication, a
tag, or release, and does not by itself establish reproducible artifact bytes.

The Phase-117 compatibility qualification uses the real Maven Central `0.2.0`
artifacts and a collision-safe task-local build of this source tree. On the
tested surface, existing `0.2.0` consumers remain source-, JVM-linkage-, and
TASTy-compatible on Scala 3.3.8 and 3.8.4. The new statement-list `Block`
surface itself did not exist in released `0.2.0`; code written against an
unpublished Term-only block-prefix candidate must migrate to the statement
supertype. These bounded results do not weaken the accepted new-minor policy
or promise whole-library compatibility. See the
[statement-ADT compatibility result](PHASE117_STATEMENT_ADT_NEW_0X_BINARY_TASTY_COMPATIBILITY_RESULT.md).
