# Versioning and stability

The project uses experimental 0.x `early-semver` metadata.

- Breaking changes to intentionally public `core` or `frontend` surfaces
  increment the minor version.
- Patch releases within one minor line are expected to preserve source and
  binary use of that line; uncertain changes use a new minor.
- No compatibility is promised across different 0.x minor lines.
- Additive overloads, givens, and extensions still require human source-review.
- All exact-compiler `frontend` artifacts share one library version.

The latest immutable release is `0.2.0`. The current source is the unpublished
`0.3.0-SNAPSHOT` development line. Its statement-based block/local-value
model changes public API shape and therefore requires a new experimental minor;
released `0.2.0` artifacts and their
[618-row API baseline](api-baselines/0.2.0.tsv) remain unchanged.

The [public API shape review](API_COMPATIBILITY_REVIEW.md) is deterministic
source/API-shape evidence, not a binary guarantee. The
[0.2-to-0.3 statement-ADT compatibility report](STATEMENT_ADT_0_2_TO_0_3_COMPATIBILITY.md)
separately records bounded source, JVM-linkage, and TASTy tests against real
Maven Central `0.2.0` artifacts.

## Module-specific compatibility

- `core` uses `_3`, stays compiler-free, and makes no compatibility promise
  across library minor lines.
- `frontend` is full-crossed by exact Scala compiler version; consumers must
  use the matching compiler-line artifact.
- `neutralScalameta` is binary-crossed but experimental and unpublished.
- `hybridScalametaFrontend` is full-crossed, explicit opt-in, experimental,
  and unpublished. Ordinary `qr`/`qq` and `tqr`/`tqq` remain current-Dotty.
- `dottyInternal` is exact-compiler integration source and unpublished.
- the aggregate and examples are unpublished.

Passing Scala 3.3.8, 3.8.4, or a 3.9.0-RC1 forward probe is evidence for the
tested revisions only. It is not a promise about later TASTy, reflection,
parser, or compiler-internal behavior. Local staging likewise does not
authorize a remote snapshot, tag, or release.
