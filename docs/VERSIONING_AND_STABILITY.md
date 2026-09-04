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

The accepted current source also replaces the public Scala/TASTy declaration
of `dqq` with a same-spelling transparent-inline selector for scalable
structural specialization. That is a new experimental 0.x-minor-class change
when released even though a source-hidden bridge preserves the old erased JVM
descriptor on all three required compiler lines. The distinction is explicit:
JVM linkage evidence does not imply Scala source or TASTy compatibility.

## Module-specific compatibility

- `core` uses `_3`, stays compiler-free, and makes no compatibility promise
  across library minor lines.
- `frontend` is full-crossed by exact Scala compiler version; consumers must
  use the matching compiler-line artifact.
- `neutralScalameta` is binary-crossed but experimental and unpublished.
- `hybridScalametaFrontend` is full-crossed, explicit opt-in, experimental,
  and unpublished. Ordinary `qr`/`qq` and `tqr`/`tqq` remain current-Dotty.
- `dottyInternal` is exact-compiler integration source and unpublished.
- the aggregate and examples are non-published build-only projects.

The first three experimental modules above are normally publishable production
projects in the candidate `0.3.0` artifact topology for Scala
3.3.8/3.8.4/3.9.0, but remain remotely unpublished. Candidate Maven
availability would not create a 1.x-style stability promise; `dottyInternal`
retains exact-line coupling and only its documented foreign-package bridges are
intended consumer seams.

Passing required Scala 3.3.8, 3.8.4, or final 3.9.0 lanes is evidence for the
tested revisions only. It is not a promise about later TASTy, reflection,
parser, or compiler-internal behavior. Local staging likewise does not
authorize a remote snapshot, tag, or release.
