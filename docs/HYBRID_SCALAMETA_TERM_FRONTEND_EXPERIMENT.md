# Hybrid Scalameta typed frontend experiment

`hybridScalametaFrontend` is an unpublished, compiler-coupled opt-in source
frontend for typed Term and Type construction and matching. It exists beside
the released/default current-Dotty frontend; public ordinary `qr`/`qq` and
`tqr`/`tqq` still use current-Dotty.

The [canonical architecture](ARCHITECTURE.md) owns the durable default,
semantic-model, fallback, and parity status. This experiment document records
route-specific mechanics and does not define a separate engine policy.

## One shared semantic model

For Terms, the module parses public `scala.meta.Term` trees and projects them
into the existing project-owned `TermShape`, `TermPattern`, templates, and
matcher. For Types, it maps `scala.meta.Type` directly into the existing
`TypeShape`, `TypeNormalForm`, `TypeTemplate`, and `TypePattern` pipeline.
Neither route prints a Scalameta tree for normal reparsing and neither creates
a second semantic model.

The supported overlapping slice is checked differentially against the
current-Dotty reference implementation on Scala 3.3.8 and 3.8.4. Type coverage
includes names, recursive fixed `List`/`Option`/`Either`, Tuple2/Tuple3,
Function1/Function2, ordered reflected holes and captures, programmatic
repeated holes, mismatches, and controlled failures. Successful captures are
the caller's original reflected subtrees.

Term construction also accepts a caller-owned `TypeRepr` only as the complete
constructor Type of the bounded one-list `new` form. `TypeRepr.of[T]`,
`TypeTree.of[T].tpe`, and a current `tqr` result share that transport. The
reflected payload is restored structurally after Scalameta parsing; wrong
positions and lowering failures are terminal and never select current-parser
fallback.

Parity means equivalent semantics where both frontends advertise support. It
does not require every future feature to be delivered in lock-step.

## Fail-closed fallback

Only `SCALAMETA_PARSE_FAILURE` may select the unchanged current parser. Exact
compiler rejection, unsupported Scalameta mapping, splice inspection failure,
target inspection failure, and construction/lowering failure are terminal and
categorized. This prevents fallback from silently widening the accepted
language or hiding a semantic failure.

Scala 3.8.4 selects Scalameta `Scala38`; Scala 3.3.8 uses the compatible
standard `Scala3` dialect available in Scalameta 4.17.3. Accepted source must
also pass the active exact compiler grammar.

The module is `publish / skip := true`. Its public experimental package is
limited to explicit `quasiquotes.scalameta` import hosts and the compact
`TermFrontend`/`TypeFrontend` programmatic boundaries. Research lowerers,
selectors, dialect policy, and parity inventories remain package-private.
See [Scalameta opt-in artifact topology](SCALAMETA_OPT_IN_ARTIFACT_TOPOLOGY.md).

## Minimal typed use and deliberate mixing

Inside a macro implementation, the current route remains:

```scala
import quasiquotes.construct.Quasiquotes.qr
import quasiquotes.matching.QuasiPattern.qq
```

The explicit Scalameta route is selected only by:

```scala
import quasiquotes.scalameta.ScalametaQuasiquotes.qr
import quasiquotes.scalameta.ScalametaQuasiPattern.qq
```

For the overlapping addition slice, each pair can build and match the same
caller-owned reflected operands. The routes also compose deliberately: a Term
built by current `qr` can be inspected by Scalameta `qq`, and a Term built by
Scalameta `qr` can be inspected by current `qq`. This is tested overlap, not a
promise that arbitrary future syntax can be mixed or that one route silently
rescues the other.

The typed opt-in currently exposes only `qr`/`qq` and `tqr`/`tqq`. It does not
expose typed Scalameta `dqr`/`dqq`; direct Scalameta definition quasiquotes in
the neutral module are source AST authoring, not reflected Definition
construction or placement.
