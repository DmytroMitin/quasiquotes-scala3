# Hybrid Scalameta typed frontend experiment

`hybridScalametaFrontend` is an unpublished, compiler-coupled opt-in source
frontend for typed Term, Type, and bounded Definition construction and
matching. It exists beside the released/default current-Dotty frontend; public
ordinary `qr`/`qq` and `tqr`/`tqq` still use current-Dotty.

The [canonical architecture](ARCHITECTURE.md) owns the durable default,
semantic-model, fallback, and parity status. This experiment document records
route-specific mechanics and does not define a separate engine policy.

## One shared semantic model

For Term construction, the module parses public `scala.meta.Term` trees and
lowers them directly into caller-owned reflected Terms in the active `Quotes`
universe. Term matching projects into the existing project-owned
`TermPattern` and matcher. For Types, it maps `scala.meta.Type` directly into the existing
`TypeShape`, `TypeNormalForm`, `TypeTemplate`, and `TypePattern` pipeline.
Neither route prints a Scalameta tree for normal reparsing and neither creates
a second semantic model.

For the bounded Definition overlap, Scalameta admits exactly one ordinary
fixed-name method with one ordinary fixed-name parameter. Construction places
two collision-free placeholders only in the complete declared-Type fields and
then forwards the original `TypeRepr` objects to the existing current-Dotty
Definition lowerer. Matching places one collision-free sentinel in the
complete RHS and creates the existing `SingleParameterDefinitionPattern` from
validated names and `TypeNormalForm` values. The route neither prints/reparses
through the handwritten Definition parser nor uses the neutral or exact
Definition backends. Accepted single-parameter construction passes complete
Tuple2/Tuple3, Function1/Function2, and current-policy nested
`List`/`Option`/`Either` `TypeNormalForm` values through the bounded
package-private Definition seam; it does not widen public `CompletedType`.
Typed-Scalameta exact-two `dqr`/`dqq` remains separate pending work.

The supported overlapping slice is checked differentially against the
current-Dotty reference implementation on Scala 3.3.8, 3.8.4, and final 3.9.0. Type coverage
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

## Integer/infix overlap with the neutral projector

The fixed no-hole family of semantic integer literals and ordinary binary
infix expressions is checked across current `qr`, opt-in Scalameta `qr`, the
programmatic `TermFrontend`, the compiler-free `ScalametaTermProjection`, and
the current parser shape. Arithmetic and comparison results agree in type and
runtime value, recursive precedence shapes agree, and the admitted typed route
uses `Engine.Scalameta` with no primary failure on Scala 3.3.8, 3.8.4, and
final 3.9.0.

That common evidence is a differential contract, not a shared lowering
implementation. The neutral projector returns a compiler-free `TermShape` plus
one truthful root span. Its original no-hole `Lit.Int`/ordinary one-RHS
`ApplyInfix` overlap now composes with direct Identifier, Select, and one-list
Apply syntax, but it still performs no typed lowering or reflected identity
transport. The typed route is active-`Quotes` lowering with reflected holes,
compiler member selection, typed failures, and parse-only fallback policy. It
also currently accepts some Scalameta infix AST topologies that the neutral
projector rejects. Routing the typed overlap or the new neutral call family
through the projector would therefore change admission, diagnostics, fallback,
and caller-owned identity contracts without an existing suitable
`TermShape -> q.reflect.Term` backend. The direct typed lowering remains, with
shared parity tests as the consolidation boundary.

## Fail-closed fallback

Only `SCALAMETA_PARSE_FAILURE` may select the unchanged current parser. Exact
compiler rejection, unsupported Scalameta mapping, splice inspection failure,
target inspection failure, and construction/lowering failure are terminal and
categorized. This prevents fallback from silently widening the accepted
language or hiding a semantic failure.

Scala 3.8.4 selects Scalameta `Scala38`; Scala 3.3.8 uses the compatible
standard `Scala3` dialect available in Scalameta 4.17.3. Accepted source must
also pass the active exact compiler grammar.

The module remains skipped in ordinary builds and becomes publish-enabled only
under the explicit `-Dquasiquotes.expandedRelease=true` candidate-release
mode. It is not remotely released today. Its public experimental package is
limited to explicit `quasiquotes.scalameta` import hosts and the compact
`TermFrontend`/`TypeFrontend` programmatic boundaries plus the two bounded
Definition interpolators. Research lowerers,
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

The same hosts expose bounded `dqr`/`dqq` for the exact current-Dotty
single-parameter identity-Definition overlap. They preserve the current typed
owner/binder, complete `TypeNormalForm`, and original RHS capture contracts.
They do not yet expose current-Dotty exact-two Definition parity. Direct
Scalameta definition quasiquotes in the neutral module remain source AST
authoring, not reflected Definition construction or placement.
