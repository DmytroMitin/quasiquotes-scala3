# Scalameta opt-in artifact topology

Phase 110 selects a future two-coordinate Scalameta topology while keeping all
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
| typed Term opt-in | `com.github.dmytromitin:quasiquotes-scala3-scalameta-frontend_<exact-scala>:<future-version>` | full Scala version | matching `frontend_<exact-scala>`, `neutral-scalameta_3` |
| package-friend peer backend | `com.github.dmytromitin:quasiquotes-scala3-dotty-internal_<exact-scala>:<future-version>` | full Scala version | `neutral-scalameta_3`, matching `scala3-compiler_3` |

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

- `ScalametaQuasiquotes.*` supplies the opt-in `qr` extension;
- `ScalametaQuasiPattern.*` supplies the opt-in `qq` extractor;
- `TermFrontend` supplies programmatic construction/pattern compilation and
  observable `Scalameta` versus `CurrentDottyFallback` results;
- `ScalametaTermPatternExtractor` is the bounded ordered-capture extractor
  returned by the opt-in `qq` host.

The research lowerers, selector, dialect policy, parity inventory, and evidence
macros remain `private[quasiquotes]`. This prevents their provisional package
layout from becoming the intended consumer surface. The existing `core` and
`frontend` sources and their 618-row API inventory are unchanged.

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

## Peer backend boundary

The historical nightly proof is retained as evidence but not reused as a
stable coordinate promise. On Scala 3.8.4, a clean package-friend consumer can
resolve the full-cross internal backend coordinate, project the neutral
`Show.apply` method, and call
`quasiquotes.definitions.dotty.PublicContextualMethodGeneratedOriginAdapter`.
It receives a positioned `untpd.DefDef` with deterministic virtual source,
complete method span, exact method/type/contextual-parameter flags, and
`NoSymbol` before insertion.

The entry point remains `private[quasiquotes]`. A later Macro-Paradise handler
must therefore be packaged under a reviewed `quasiquotes.*` friend namespace,
as in the historical proof. Macro-Paradise continues to own annotation
lifecycle, companion merge, placement, insertion, rollback, and typing. No
public `u*` family and no public exact-`tpd` family is created.

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
