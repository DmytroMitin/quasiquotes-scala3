# quasiquotes-scala3

An experimental Scala 3 library for structural quasiquote research. The
repository separates compiler-free representations and algorithms from
compiler-coupled parsing, reflection, and lowering.

The project is a research proof of concept. Its API, coordinates, supported
syntax, and compatibility policy may change. The immutable `0.2.0` `core` and
matching 3.3.8/3.8.4 `frontend` artifacts are available from Maven Central;
the current `0.3.0-SNAPSHOT` tree is not published.

The canonical [architecture](docs/ARCHITECTURE.md) has one project-owned,
compiler-free semantic model with multiple source frontends. Current-Dotty is
the released/default reference route; the Scalameta typed route is an explicit,
unpublished experiment rather than a second quasiquote engine.

## Quick start

`qr` constructs a Scala 3 quoted-reflection `Term` from source-like syntax
with structural splices.

For an explicit receiver whose member name is computed during macro expansion,
`SelectedMemberName.from(decoded)` provides a validated compiler-free name
value that can occupy only the selection-name slot, for example
`qr"$receiver.$selectedName($argument)"`. Ordinary `String` values are not
name holes, and this surface does not perform lexical or symbol lookup by
string.

<!-- snippet:readme-quick-start:start -->
```scala
import scala.quoted.*
import quasiquotes.construct.Quasiquotes.*

object ReadmeQuickStart:
  inline def add(left: Int, right: Int): Int = ${ addImpl('left, 'right) }

  private def addImpl(left: Expr[Int], right: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    addImplTerm(left.asTerm, right.asTerm).asExprOf[Int]

  private def addImplTerm(using q: Quotes)(left: q.reflect.Term, right: q.reflect.Term): q.reflect.Term =
    qr"$left + $right"
```
<!-- snippet:readme-quick-start:end -->

The layers are explicit: the inline macro receives typed staged `Expr[Int]`
values, converts them to low-level `quotes.reflect.Term` trees, uses `qr` for
source-like structural construction at that reflection-tree layer, and converts
the resulting `Term` back to `Expr[Int]`. A `quotes.reflect.Term` is generally a
typed quoted-reflection tree in macro use; it is not the compiler-internal raw
`dotty.tools.dotc.ast.untpd.Tree` used only by the unpublished `dottyInternal`
module.

This exact example is compiled from an external-package fixture. See
[Getting started](docs/GETTING_STARTED.md) for the larger construction,
matching, type, Lambda1, P1/P2 block, source-owned local-definition, and
compiler-free definition examples. See
[execution environments and AST representations](docs/EXECUTION_ENVIRONMENTS_AND_AST_REPRESENTATIONS.md)
for compile-time macros, runtime staging, compiler-free values, and
compiler-backed parsing without `Quotes`.

## Quasiquote surfaces

<!-- public-surface-table:start -->
| Role | Interpolated syntax | Interpolator availability | Programmatic API | Function/API availability |
| --- | --- | --- | --- | --- |
| Term construction | `qr"..."` | Public now | `QuasiquoteBuilder.build(...)` | Public now |
| Term pattern matching | `case qq"..."` | Public now | `QuasiPattern.term(...)`, `termOrThrow(...)` | Public now |
| Type construction | `tqr"..."` | Public now | `QuasiTypequotes.tqr(...)` | Public research API |
| Type pattern matching | `case tqq"..."` | Public now | `QuasiTypequotes.tqq(...)` / `QuasiTypePattern.*` | Public research API |
| Definition construction | `dqr"def id(x: $parameterType): $resultType = x"` | Public now, exact bounded shape | `DefinitionConstruction.*` | Public bounded compiler-free API |
| Definition pattern matching | `case dqq"def id(x: Int): Int = $body"` | Public now, exact bounded shape | `DefinitionPattern.singleParameter(...)` | Public now, exact bounded shape |
<!-- public-surface-table:end -->

`qr` is the ergonomic aborting term-construction syntax;
`QuasiquoteBuilder.build` is its recoverable programmatic counterpart. The
bounded `qq` extractor returns caller-owned `quotes.reflect.Term` captures in
left-to-right slot order. It admits term slots only, treats every slot as
distinct, returns ordinary mismatch through pattern fallthrough, and reports a
malformed template during macro expansion. Use `QuasiPattern.term` or
`termOrThrow` for explicit diagnostics and named/repeated-hole semantics.

`qr` also accepts a caller-owned `quotes.reflect.TypeRepr` as the complete
constructor Type of its bounded one-list `new` form and as each complete Type
in the bounded source-owned local-definition form. `TypeRepr.of[T]`,
`TypeTree.of[T].tpe`, and a `tqr` result use the same transport, including
direct `tqr"java.lang.StringBuilder"` to `qr"new $typeValue(arg)"` stacking.
Passing `Type[T]` or `TypeTree` directly, partial/applied dynamic Type
positions, and variadic arguments remain unsupported.

The type names are intentionally layered overloads. Inside an active `Quotes`,
`tqr"..."` accepts caller-owned `TypeRepr` splices and returns a caller-owned
`TypeRepr`; `case tqq"..."` returns original target subtrees in source-slot
order. The same imports retain the recoverable neutral functions
`QuasiTypequotes.tqr(...)` and `QuasiTypequotes.tqq(...)`. The interpolated
slots are distinct ordinal positions, while named and repeated-hole semantics
remain available through the programmatic API. `DefinitionConstruction.*` is
bounded compiler-free semantic construction/projection. The public `dqr`
interpolator is a separate caller-owned Quotes surface: it admits exactly one
ordinary parameter, two equal supported `TypeRepr` splices, and a literal body
that names that parameter. It returns a `DefDef` owned by the current
`Symbol.spliceOwner` for immediate placement in the same macro-generated local
block. It is not a detached tree, body-hole API, or general owner/placement
facility.

No umbrella import façade is public in this snapshot. A test-scoped design
probe shows that an additive façade can directly export the existing six
interpolators when used in a real macro `Quotes` universe. The current domain
imports above remain the compatibility baseline until a separate
implementation gate.

The public `dqq` extractor and programmatic
`DefinitionPattern.singleParameter(...)` matcher admit one fixed ordinary
method name, one fixed ordinary parameter and fixed bounded parameter/result
types, with `$body` as the complete RHS. A valid pattern returns `None` for an
ordinary target mismatch and captures the caller's exact original RHS
`q.reflect.Term` on success. The programmatic result additionally preserves
the original reflected parameter and result types.

See the [syntax support matrix](docs/SYNTAX_SUPPORT_MATRIX.md) for the current
construct/match boundary and its deliberate limits.

## Related projects

- [Macro-Paradise for Scala 3](https://github.com/DmytroMitin/macroparadise-scala3)
  is an experimental Scala 3 pre-typer macro-annotation compiler plugin;
  Quasiquotes integration is optional research, not a core product dependency.
- [AUXify-scala3](https://github.com/DmytroMitin/AUXify-scala3) is an
  experimental Scala 3 AUXify reimplementation using Macro-Paradise; its
  current narrow handler path uses Scalameta source-like authoring plus the
  Quasiquotes exact contextual-method lowering bridge.

## Modules

- `core` contains compiler-free term/type/definition values, construction,
  matching, source metadata, and stable diagnostic projections.
- `frontend` supplies Scala 3 compiler-coupled parsing, macros, quoted
  reflection adapters, and public source-oriented conveniences.
- `neutralScalameta` is an unpublished compiler-free experiment backed by
  Scalameta 4.17.3. It provides direct source-AST authoring plus a bounded
  structural projection into the existing validated IR, without `Quotes`,
  compiler implementation dependencies, staging, SemanticDB, or exact trees.
- `hybridScalametaFrontend` is an unpublished, compiler-coupled side-by-side
  experiment. It contains explicit typed Term and Type opt-in APIs in
  `quasiquotes.scalameta`. Both parse public Scalameta ASTs, lower into existing
  project semantics, and retain current-Dotty as the reference/oracle. Only a
  Scalameta parse failure may use the current parser as fallback; semantic or
  lowering failures remain fail-closed.
  Public `qr`/`qq` and `tqr`/`tqq` defaults and published dependencies do not
  change.
  It has no typed `dqr`/`dqq`; neutral Scalameta definition authoring and typed
  reflected Definition placement are distinct contracts.
- `dottyInternal` contains exact-compiler internal adapters and one narrow
  experimental foreign-package peer bridge for contextual-method lowering.
  Its source is present for review and testing, but its artifact is deliberately
  unpublished and consumers must match the exact Scala compiler version.
- `public-core-examples` and `public-api-examples` compile consumer code from
  outside the library packages.

In role, `frontend` is closest to Scala 2 quasiquotes: it owns source-like
quotation/pattern syntax and compiler-coupled construction and matching.
`core` remains a small project-owned validated structural model rather than a
full Scala AST. The experimental Scalameta layer sits above it and projects
only admitted shapes downward; it does not make `core` depend on Scalameta.
See the [neutral Scalameta experiment](docs/NEUTRAL_SCALAMETA_EXPERIMENT.md),
the [hybrid typed frontend experiment](docs/HYBRID_SCALAMETA_TERM_FRONTEND_EXPERIMENT.md),
the [Scalameta opt-in artifact topology](docs/SCALAMETA_OPT_IN_ARTIFACT_TOPOLOGY.md),
and the [Dotty-internal exact backend](docs/DOTTY_INTERNAL_BACKEND.md), whose
only foreign-package surface is the focused experimental contextual-method
peer bridge.

## Try the source build

Requirements are JDK 25 and sbt 1.12.15. The required baseline is Scala 3.8.4.

```sh
sbt -batch clean test publicCoreExamples/test publicApiExamples/test \
  core/verifyCoreBoundary neutralScalameta/verifyNeutralScalametaBoundary \
  verifyModuleGraph package
```

The build serializes tasks and uses exported test/compile JARs with flat test
class-loader layering to keep the aggregate gate deterministic.

## Latest released coordinates

The latest Maven Central release is the immutable version `0.2.0`:

```scala
libraryDependencies +=
  "com.github.dmytromitin" %% "quasiquotes-scala3-core" % "0.2.0"

libraryDependencies +=
  "com.github.dmytromitin" %
    "quasiquotes-scala3-frontend_3.8.4" % "0.2.0"
```

`core` uses ordinary Scala 3 binary crossing. `frontend` uses full compiler
version crossing and must match the consuming compiler line. The released set
contains `core_3` plus frontend artifacts for Scala 3.3.8 and 3.8.4 only. The
current source tree is the unpublished development version `0.3.0-SNAPSHOT`
and is not interchangeable with these released coordinates.

See [Getting started](docs/GETTING_STARTED.md),
[execution environments and AST representations](docs/EXECUTION_ENVIRONMENTS_AND_AST_REPRESENTATIONS.md),
[diagnostics](docs/DIAGNOSTICS.md),
[architecture](docs/ARCHITECTURE.md),
[neutral Scalameta experiment](docs/NEUTRAL_SCALAMETA_EXPERIMENT.md),
[hybrid typed frontend experiment](docs/HYBRID_SCALAMETA_TERM_FRONTEND_EXPERIMENT.md),
[syntax support matrix](docs/SYNTAX_SUPPORT_MATRIX.md),
[exact constructor backend](docs/EXACT_BACKEND_CONSTRUCTOR_NEW.md),
[supported syntax and limitations](docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md),
[why quasiquotes?](docs/WHY_QUASIQUOTES.md),
[north-star quasiquote examples](docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md),
[compatibility](docs/COMPATIBILITY.md),
[public API shape compatibility review](docs/API_COMPATIBILITY_REVIEW.md),
[statement-ADT 0.2-to-0.3 compatibility qualification](docs/STATEMENT_ADT_0_2_TO_0_3_COMPATIBILITY.md),
[versioning and stability](docs/VERSIONING_AND_STABILITY.md), and the
[release process](docs/RELEASE_PROCESS.md).

The machine-readable [0.2.0 public API baseline](docs/api-baselines/0.2.0.tsv)
contains 305 core and 313 frontend Scaladoc-visible entries. It excludes the
root, unpublished experimental `neutralScalameta`, unpublished
`hybridScalametaFrontend`, unpublished `dottyInternal`, and package-private
internals. It is generated from packaged Scaladoc search metadata for
deterministic source/API-shape diffing; it is neither human API documentation
nor binary, TASTy, overload-resolution, or semantic compatibility proof.

The structural type subset includes recursively nested `List` and `Option`
applications plus binary `Either`, including patterns, construction, quoted
lowering/inspection, typed ascriptions, and scoped type evidence. Constructor
admission remains fixed. An experimental programmatic
`GlobalSelectedTypeFrontend` accepts canonical globally addressable selected
names only through an explicit `GlobalSelectedTypeEnvironment` built from
typed witnesses. It supports selected standard `List`/`Option`/`Either` by
full declaration identity; it does not add general name resolution, aliases,
stable-term paths, or ambient lookup. Interpolated `tqr` additionally admits a
zero-hole canonical globally selected class terminal such as
`java.lang.StringBuilder`, resolved through an exact typed witness; this does
not admit aliases, stable-term paths, or selected constructor applications,
and `tqq` remains unchanged.

The canonical first-use examples, including the complete Lambda1, bounded P1
block and single-typed-local-val P2 `qr`/`qq`, and bounded `tqr`/`tqq` macro paths, are mirrored from compiled
external-package fixtures, and the repository's snippet drift check compares
them byte for byte.
Public type diagnostics describe the supported boundary without development
chronology or generated placeholder names.

The compiler-free public API constructs bounded single- and exact-two-parameter
methods whose bodies explicitly select a declared parameter. That public name
selection is converted once to the package-private binder-aware definition
core; a free same-text `CompletedTerm.reference` is never captured implicitly.
This is a semantic construction/projection API, not a source parser or
method-placement backend.

## License

This project is licensed under the Apache License, Version 2.0. See
[LICENSE](LICENSE). Repository visibility and later artifact publication
remain separate decisions.

Support expectations are intentionally conservative; see [Support](SUPPORT.md)
and [Security policy](SECURITY.md). No private security-reporting channel is
currently offered or promised for this experimental research stage. Do not
post sensitive material publicly merely to obtain maintainer attention.

The experimental frontend also supports bounded fully-qualified constructor
expressions such as `new java.lang.StringBuilder(16)` for `qr` construction and
structural patterns. See the supported-syntax document for the deliberate
generic, imported-name, multiple-list, and anonymous-class exclusions.

One ordinary explicitly typed Lambda1 form is also available for structural
`qr` construction and matching. Its project-owned binder identity provides
alpha-aware bound-reference comparison and same-text splice non-capture. The
unpublished exact internal backend supports that same bounded Lambda1 shape in
source-free and generated-origin modes.

Binder-free P1 blocks are also available through ordinary `qr` construction
and `qq`/programmatic matching. They preserve one or more ordered expression
prefixes and a distinct final result. P2 separately admits one typed eager
immutable local `val`. Construction-only source-owned local-definition support
now admits exactly one literal method with one ordinary parameter, complete
parameter/result `TypeRepr` holes, a parameter-reference body, and one following
result; broader statements and `qq` local-definition matching remain excluded.

The unpublished exact internal definition backend also supports the bounded
single-ordinary-parameter and exact-two-ordinary-parameter definition shapes
behind the compiler-free public projections. It constructs source-free raw
trees or deterministic generated-origin trees without parsing in production,
resolving parameter references by project binder identity rather than display
text. This does not expose compiler trees publicly, generalize parameter-list
syntax, or add general method placement/owner support.
