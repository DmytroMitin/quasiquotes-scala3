# Compatibility

The required source-build matrix is Scala 3.3.8 (previous LTS), Scala 3.8.4
(previous stable), and final Scala 3.9.0 (current LTS) on JDK 25 with sbt
1.12.15. The development default remains 3.8.4; it does not define the full
support matrix. The test dependency is MUnit 1.2.4.

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
  experimental and published in 0.3.0; compatibility is tested on Scala 3.3.8,
  3.8.4, and 3.9.0 and remains bounded by Scalameta dialect support;
- `hybridScalametaFrontend`: full Scala-version crossing, released as an
  explicit experimental opt-in in 0.3.0;
- `dottyInternal`: full Scala-version crossing, published in 0.3.0, with an
  experimental family of definition-specific peer bridges whose
  compiler-internal output requires an exact producer/consumer compiler match;
- aggregate root and example modules: unpublished.

The published expanded `0.3.0` release set contains the binary-crossed neutral
artifact plus 3.3.8/3.8.4/3.9.0 full-crossed frontend, Scalameta-frontend, and
exact-backend artifacts. The two
binary-cross artifacts are built with the oldest supported line, Scala 3.3.8,
so all three advertised compilers can consume the same `_3` bytes. Publication
does not turn the exact backend into a stable raw-tree API.

Versions `0.2.0` and `0.3.0` under group `com.github.dmytromitin` identify
immutable released Maven Central coordinates. The active development tree is
`0.4.0-SNAPSHOT`; no 0.4.0 artifact is published. The selected experimental
0.x policy requires breaking changes to increment the minor version and expects
patch compatibility within one minor line; see
[Versioning and stability](VERSIONING_AND_STABILITY.md).

The checked public API inventory and passing source/consumer lanes are review
evidence only. They do not establish binary compatibility, cross-line frontend
compatibility, or a stable 1.x API.

The bounded public `qq` extractor begins in the `0.2.x` source line. Code or
TASTy compiled against the former `qq: Nothing` signature must be rebuilt and
adapted; the intentional replacement is not a patch-compatible `0.1.x` change.

The bounded public `tqr` interpolator and scalar `tqq` extractor entered the
`0.2.x` source line. A sequence-shaped `tqr` overload preserves the existing
varargs function's eta-expanded method-value shape beside the same-named
interpolator. The active ranked-Type change makes the public Scala/TASTy `tqq` declaration to a
same-spelling transparent-inline selector so one static fixed-constructor
argument slot can bind `Seq[q.reflect.TypeRepr]`, while scalar slots still bind
exact `q.reflect.TypeRepr`. The historical scalar JVM descriptor returning
`TypePatternExtractor` is preserved through a source-hidden bridge; this does
not preserve TASTy identity. Reflected construction results and captures remain
owned by the caller's active `Quotes`; they are not cross-compiler or
cross-Quotes portable.

The opt-in typed-Scalameta frontend mirrors that bounded selector change. Its
hybrid inventory remains 45 rows / 43 groups: exactly one existing `tqq`
source/TASTy signature is replaced, with no hybrid-specific public carrier or
helper. The historical erased JVM descriptor remains
`(Lscala/StringContext;Lscala/quoted/Quotes;)Lquasiquotes/scalameta/ScalametaTypePatternExtractor;`.
As on the standard frontend, this preserves scalar JVM linkage but not the old
TASTy declaration identity.

The bounded public `dqr` interpolator is additive in the `0.2.x` source line.
Its result is a caller-owned `DefDef` under the current `Symbol.spliceOwner`,
tested only for immediate same-Quotes local-block placement. It is not a binary
or semantic promise across compiler lines, a detached-tree format, or a general
owner/placement API. The package-private pre-existing definition parser and
exact internal backends retain their separate contracts. Its variadic signature
is unchanged while current-Dotty semantics additionally admit the bounded
exact-two ordinary-parameter Int/String/Boolean slice; that is a semantic
widening, not general N-parameter Definition parity.

The public Scala/TASTy `dqq` declaration changes from the concrete exact-one
return to a same-spelling transparent-inline selector so static structural
templates can specialize to scalable `DefinitionPatternExtractor`. Static
exact-one use retains `SingleParameterDefinitionPattern`, and dynamic/non-static
use retains the historical exact-one fallback. This replacement is an
experimental 0.x-minor-class change when eventually released: source/TASTy
consumers must be recompiled and reviewed. Independently, the historical erased
JVM descriptor returning `SingleParameterDefinitionPattern` is preserved on
Scala 3.3.8, 3.8.4, and 3.9.0 by a source-hidden bridge. JVM-linkage preservation
does not erase the Scala/TASTy API-shape change.

The neutral experiment uses the imported standard `Scala3` dialect singleton
for compile-time quasiquotes. Explicit parser calls select `Scala38` or
`Scala3Future` values separately. This is syntax compatibility evidence only:
the module does not promise semantic resolution of every parsed form.
