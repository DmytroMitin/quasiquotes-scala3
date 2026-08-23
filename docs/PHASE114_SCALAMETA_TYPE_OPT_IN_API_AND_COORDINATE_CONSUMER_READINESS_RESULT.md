# Scalameta Type opt-in API and coordinate readiness result

This gate exposes the already-proven bounded Type-Q3 implementation through
the existing remotely unpublished full-cross Scalameta frontend coordinate.
It does not change the ordinary public Type frontend.

## Explicit surface

The selected A1 shape extends the existing Scalameta hosts:

```scala
import quasiquotes.scalameta.ScalametaQuasiquotes.*
val constructed = tqr"Either[List[$left], Option[$right]]"

import quasiquotes.scalameta.ScalametaQuasiPattern.*
target match
  case tqq"Either[List[$left], Option[$right]]" => (left, right)
```

`quasiquotes.scalameta.TypeFrontend` is the detailed programmatic boundary. It
exposes `Engine`, compact `Failure`, `BuildResult`, `CompileResult`,
`MatchResult`, the selected dialect name, Quotes-aware construction, pattern
compilation, and matching that returns original caller-owned `TypeRepr`
subtrees. The Scalameta mapper, hybrid selector, dialect implementation, and
parity inventory remain package-private.

The admitted syntax remains exactly the accepted private parity matrix: names, recursive
fixed `List`/`Option`/`Either`, Tuple2/Tuple3, Function1/Function2, ordered
interpolation slots, and programmatic repeated holes. Selected/path-dependent
types, broader constructors/arities, wildcards/bounds, refinements,
unions/intersections, match/type-lambda families, semantic equality/subtyping,
and constructor-position holes remain unsupported.

## Failure and fallback boundary

Public failures preserve the categories `SCALAMETA_PARSE_FAILURE`,
`EXACT_COMPILER_SYNTAX_REJECTED`, `SCALAMETA_TYPE_LOWERING_UNSUPPORTED`,
`TYPE_SPLICE_INSPECTION_FAILURE`, `TYPE_TARGET_INSPECTION_FAILURE`, and
`TYPE_CONSTRUCTION_FAILURE`. Only Scalameta parse failure may use the retained
current parser. The bounded supported-line matrix has no real dialect lag, so
the retained controlled internal test proves the fallback branch while fresh
consumers observe the selected Scalameta engine and the public fallback enum.

## Local coordinate evidence

A task-owned Maven repository outside the checkout contains synthetic version
`0.3.0-phase114-local`. Fresh Scala 3.3.8 and 3.8.4 consumers resolve only
`com.github.dmytromitin:quasiquotes-scala3-scalameta-frontend_<exact-scala>`
plus normal repositories and pass 7/7 tests on each line. The compile closure
is 24 JARs and truthfully includes matching `frontend_<exact>`, binary-cross
`neutral-scalameta_3`, `core_3`, Scalameta 4.17.3, and the compiler closure.

The six synthetic coordinates produce 24 nonempty POM/binary/source/Javadoc
files (72 repository files including checksums). POMs retain Apache-2.0,
project URL/SCM, full-cross typed coordinates, and binary-cross neutral/core
dependencies. No `ProjectRef`, checkout source/class directory, control path,
root/example artifact, signing, Central request, or remote publication occurs.

## Result boundary

```text
TYPE_Q3_OPT_IN_API_AND_COORDINATE_READY
PUBLIC_TQR_TQQ_DEFAULT = CURRENT_DOTTY
PUBLIC_QR_QQ_DEFAULT = CURRENT_DOTTY
CURRENT_DOTTY_ENGINE_RETIREMENT = NOT_AUTHORIZED
DEFINITION_Q3_MIGRATION = NOT_AUTHORIZED
SCALAMETA_TYPE_OPT_IN_COORDINATE_REMOTE_PUBLICATION = NOT_AUTHORIZED
```

The released `0.2.0` set and its 618-row `core`/`frontend` API remain
unchanged. This local gate does not claim remote consumability.
