# Compatibility

The required source-build baseline is Scala 3.8.4 on JDK 25 with sbt 1.12.8.
The test dependency is MUnit 1.2.4.

Additional source lanes have been used as experimental evidence, including
Scala 3.3.8 and newer compiler lines. Passing lanes do not create a permanent
compatibility promise for quoted reflection, parser output, TASTy, or Dotty
internals.

Artifact policy:

- `core`: ordinary Scala 3 binary crossing, with a deliberately compiler-free
  dependency boundary;
- `frontend`: full Scala compiler-version crossing; producer and consumer
  compiler lines must match exactly (for example, a Scala 3.8.4 consumer uses
  `quasiquotes-scala3-frontend_3.8.4`, never a 3.3.8 or 3.9 artifact);
- `dottyInternal`: exact-build test source only, unpublished;
- aggregate root and example modules: unpublished.

Version `0.2.0-SNAPSHOT` and group `io.github.dmytromitin` are provisional
local-publication settings. No remote repository availability or stable release
support is promised. The selected experimental 0.x policy requires breaking
changes to increment the minor version and expects patch compatibility within
one minor line; see [Versioning and stability](VERSIONING_AND_STABILITY.md).

The checked public API inventory and passing source/consumer lanes are review
evidence only. They do not establish binary compatibility, cross-line frontend
compatibility, or a stable 1.x API.

The bounded public `qq` extractor begins in the `0.2.x` source line. Code or
TASTy compiled against the former `qq: Nothing` signature must be rebuilt and
adapted; the intentional replacement is not a patch-compatible `0.1.x` change.

The bounded public `tqr` interpolator and `tqq` extractor are additive in the
`0.2.x` source line. A sequence-shaped `tqr` overload preserves the existing
varargs function's eta-expanded method-value shape beside the same-named
interpolator. Reflected construction results and captures remain owned by the
caller's active `Quotes`; they are not cross-compiler or cross-Quotes portable.

The bounded public `dqr` interpolator is additive in the `0.2.x` source line.
Its result is a caller-owned `DefDef` under the current `Symbol.spliceOwner`,
tested only for immediate same-Quotes local-block placement. It is not a binary
or semantic promise across compiler lines, a detached-tree format, or a general
owner/placement API. The package-private pre-existing definition parser and
exact internal backends retain their separate contracts.
