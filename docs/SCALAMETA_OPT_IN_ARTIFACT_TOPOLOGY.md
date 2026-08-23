# Scalameta opt-in artifact topology

The selected future topology uses two Scalameta coordinates while keeping all
new coordinates remotely unpublished and disabled by default:

```text
SCALAMETA_OPT_IN_TWO_COORDINATE_TOPOLOGY_RECOMMENDED
PEER_EXACT_FRIEND_ARTIFACT_RECOMMENDED
READY_FOR_SEPARATE_AUXIFY_APPLY_PEER_INTEGRATION_DESIGN
```

This is a local consumability result, not release authorization. The public
`qr` and `qq` imports in the existing frontend still use the current Dotty
engine. No process-global flag, environment variable, mutable default, or
silent import replacement is introduced.

## Selected future coordinates

| Role | Future coordinate shape | Cross policy | Direct project dependencies |
| --- | --- | --- | --- |
| compiler-free bridge | `com.github.dmytromitin:quasiquotes-scala3-neutral-scalameta_3:<future-version>` | Scala 3 binary | `core_3`, Scalameta 4.17.3 |
| typed Term and Type opt-in | `com.github.dmytromitin:quasiquotes-scala3-scalameta-frontend_<exact-scala>:<future-version>` | full Scala version | matching `frontend_<exact-scala>`, `neutral-scalameta_3` |
| exact-version peer backend | `com.github.dmytromitin:quasiquotes-scala3-dotty-internal_<exact-scala>:<future-version>` | full Scala version | `neutral-scalameta_3`, matching `scala3-compiler_3` |

The first two rows are the selected user-facing topology. The third is a
separately version-coupled integration artifact, not a stable public raw-tree
API. All three modules retain `publish / skip := true`. A disposable rehearsal
may override that setting only in the active sbt session and publish a
synthetic version to a task-owned repository.

The neutral coordinate is binary-cross because its production sources contain
neither `Quotes` nor compiler implementation types. The typed coordinate is
full-cross because its API and implementation use the active `Quotes` universe
and depend on the exact full-cross frontend. The peer backend is full-cross
because `untpd`, compiler `Context`, source files, flags, and tree constructors
are compiler-implementation contracts.

## Public API boundary

The neutral coordinate intentionally exposes:

- `quasiquotes.neutral.ScalametaContextualMethodProjection`;
- `ProjectedContextualMethod`, `NeutralSourceSpan`, and
  `NeutralProjectionError`;
- the transitive public compiler-free projection values already owned by
  `core_3`.

Consumers author ordinary Scalameta trees directly. There is no copied `n*`
macro engine. A direct Scalameta definition quasiquote or parsed `Defn.Def` can
project the admitted method
`def apply[A](using inst: Show[A]): Show[A] = inst`; generated trees truthfully
report no source span.

The typed coordinate exposes only the explicit package
`quasiquotes.scalameta`:

- `ScalametaQuasiquotes.*` supplies the opt-in `qr` and `tqr` extensions;
- `ScalametaQuasiPattern.*` supplies the opt-in `qq` and `tqq` extractors;
- `TermFrontend` supplies programmatic construction/pattern compilation and
  observable `Scalameta` versus `CurrentDottyFallback` results;
- `TypeFrontend` supplies Quotes-aware Type construction, pattern compilation
  and detailed matching with the same observable engine/failure boundary;
- `ScalametaTermPatternExtractor` is the bounded ordered-capture extractor
  returned by the opt-in `qq` host;
- `ScalametaTypePatternExtractor` returns ordered original `TypeRepr` captures
  from the opt-in `tqq` host.

The research lowerers, selector, dialect policy, parity inventory, and evidence
macros remain `private[quasiquotes]`. This prevents their provisional package
layout from becoming the intended consumer surface. Released `0.2.0` retains
its immutable 618-row inventory. The Phase-116 source candidate's 634-row
statement-ADT delta belongs to the shared Term model, not to accidental
exposure from the opt-in Scalameta implementation.

The complete-current-matrix Type-Q3 implementation stays in this same
full-cross module. Its dialect policy, `scala.meta.Type` mapper, internal
template/pattern frontend, selector result, and parity inventory remain
`private[quasiquotes]`; only the compact public façade above is exposed.

Example explicit imports inside a quoted macro implementation:

```scala
import quasiquotes.scalameta.ScalametaQuasiquotes.*
val constructed = qr"$supplied.apply(1)"
```

```scala
import quasiquotes.scalameta.ScalametaQuasiPattern.*
target match
  case qq"$left + $right" => (left, right)
```

```scala
import quasiquotes.scalameta.ScalametaQuasiquotes.*
val constructedType = tqr"Either[List[$leftType], Option[String]]"
```

```scala
import quasiquotes.scalameta.ScalametaQuasiPattern.*
targetType match
  case tqq"Either[List[$left], Option[$right]]" => (left, right)
```

Importing `quasiquotes.construct.Quasiquotes.*` and
`quasiquotes.matching.QuasiPattern.*` instead continues to select the current
Dotty frontend.

## Measured dependency closure

The synthetic `0.3.0-phase110-local` rehearsal resolved from a file-URI Maven
repository outside the checkout. Compile classpaths contained:

- neutral coordinate: 12 JARs on each supported line — neutral, core,
  Scalameta, its parser/tree/common/io closure, `sourcecode`, and the selected
  Scala libraries;
- typed coordinate: 24 JARs on each supported line — typed opt-in, exact
  frontend, neutral, core, the neutral Scalameta closure, and the selected
  compiler/interface/TASTy/ASM/JLine/compiler-interface closure;
- exact 3.8.4 backend: 19 JARs — backend, neutral, core, the Scalameta closure,
  and the exact compiler closure, without the quoted frontend coordinate.

Every selected module produced a nonempty POM, binary JAR, source JAR, and
Javadoc JAR with Apache-2.0 metadata. The seven synthetic coordinates contained
no `ProjectRef`, source/class directory, controller path, checkout path, root,
example, or unsupported compiler-probe dependency.

Fresh external builds consumed only the synthetic coordinates plus normal
external repositories. The neutral tests passed 3/3 and the typed tests passed
4/4 on Scala 3.3.8 and 3.8.4. The typed consumer covered literals,
identifiers, reflected-hole identity, selection/application, construction,
Lambda1, one/multiple/generated capture identity, observable parse-only
fallback, and an unchanged current-Dotty control.

The Type API gate separately rehearsed synthetic `0.3.0-phase114-local` typed
coordinates on both supported compiler lines. Each fresh Type consumer passed
7/7 tests from a 24-JAR compile closure, covering zero/reflected `tqr`, ordered
and whole-target `tqq` identity, programmatic construction/matching, repeated
holes, controlled malformed/unsupported failures, engine metadata, and the
ordinary current-Dotty control. The task-owned repository contained six local
coordinates and 24 primary POM/binary/source/Javadoc files (72 files including
checksums), with no checkout or controller coupling. This remains local-only
evidence and does not make the coordinate remotely available.

The P2 Term gate rehearsed fresh synthetic `0.3.0-phase116-local` typed
coordinates on both supported compiler lines. Each coordinate-only consumer
passed 4/4 tests for local-val construction, alpha-insensitive matching,
initializer capture, and same-display-name external-splice noncapture. The
task-owned repository again contains six local coordinates and 24 primary
POM/binary/source/Javadoc files (72 including checksums); its POMs contain no
`ProjectRef`, checkout, control, source-directory, or class-directory coupling.
This proof is local-only and does not authorize remote publication.

## Peer backend boundary

The historical package-friend proof is retained as evidence but is superseded
for foreign-package use by
`quasiquotes.definitions.dotty.ContextualMethodPeerBridge`. On exact Scala
3.8.4, the bridge accepts only the admitted Scalameta contextual method and a
validated virtual source name. It returns a positioned `untpd.DefDef`,
deterministic generated source and virtual-source provenance, with exact
method/type/contextual-parameter flags and `NoSymbol` before insertion.

The bridge is callable from an AUXify-owned package without claiming a
`quasiquotes.*` friend namespace. It remains compiler-internal, full-cross,
experimental, and remotely unpublished. Macro-Paradise continues to own
annotation lifecycle, companion merge, placement, insertion, rollback, and
typing. No public `u*`, generic raw-tree, or exact-`tpd` family is created.

## Alternatives and release recommendation

- A single combined coordinate would prevent neutral-only authoring without
  the compiler-coupled frontend and would obscure the binary/full-cross split.
- Publishing only neutral would defer an already proven typed opt-in surface.
- Keeping all modules unpublished would discard successful coordinate-only
  closure evidence.

The two-coordinate topology is therefore preferred. If separately authorized,
the user-facing coordinates fit a `0.3.0`-style experimental expansion better
than an additive `0.2.x`: they introduce a new dependency family and new
experimental APIs. The exact backend should remain unreleased or separately
version-coupled until a real peer integration is reviewed. The released
`0.2.0` coordinate set remains immutable and the release repository checker
continues to reject additional coordinates.
