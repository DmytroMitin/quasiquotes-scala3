# Compatibility

The required source-build baseline is Scala 3.8.4 on JDK 25 with sbt 1.12.15.
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
- `neutralScalameta`: ordinary Scala 3 binary crossing, Scalameta 4.17.3,
  experimental and unpublished; compatibility is tested on Scala 3.3.8 and
  3.8.4 and remains bounded by Scalameta dialect support;
- `dottyInternal`: full Scala-version crossing, unpublished, with an
  experimental `ContextualMethodPeerBridge` whose compiler-internal input and
  output require an exact producer/consumer compiler match;
- aggregate root and example modules: unpublished.

The candidate expanded `0.3.0` release set adds the binary-crossed neutral
artifact plus 3.3.8/3.8.4 full-crossed Scalameta-frontend and exact-backend
artifacts under an explicit release-mode opt-in. Scala 3.9.0-RC1 remains
validation-only. This candidate topology does not describe a completed remote
release or turn the exact backend into a stable raw-tree API.

Version `0.2.0` and group `com.github.dmytromitin` identify the immutable
released Maven Central coordinates. The current working tree is the
unpublished `0.3.0-SNAPSHOT` development line. The selected experimental 0.x
policy requires breaking changes to increment the minor version and expects
patch compatibility within one minor line; see
[Versioning and stability](VERSIONING_AND_STABILITY.md).

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

The neutral experiment uses the imported standard `Scala3` dialect singleton
for compile-time quasiquotes. Explicit parser calls select `Scala38` or
`Scala3Future` values separately. This is syntax compatibility evidence only:
the module does not promise semantic resolution of every parsed form.
