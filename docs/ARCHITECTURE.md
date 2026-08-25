# Architecture

The durable language pipeline is:

```text
source frontend(s)
    -> project-owned compiler-free semantic model in core
    -> backend-specific lowering / reflected matching
```

There are multiple source-facing routes, but there are not multiple semantic
quasiquote engines. `core` owns the compiler-free Term and Type normal forms,
templates, patterns, binder identities, structural construction and matching
rules, and neutral diagnostics. Frontends must project into those shared
models rather than inventing frontend-local equality or binding semantics.

## Modules and dependency direction

```text
frontend ----------------> core
neutralScalameta ---------> core
dottyInternal ---> neutralScalameta ---> core
hybridScalametaFrontend ---> frontend + neutralScalameta
publicApiExamples -> frontend
publicCoreExamples -> core
```

- `frontend` is the released/default exact-compiler route. It owns parsing,
  quoted reflection, public `qr`/`qq` and `tqr`/`tqq`, and compiler-line
  lowering. It is also the first-class reference implementation and oracle.
- `neutralScalameta` is an unpublished compiler-free Scalameta 4.17.3 AST
  boundary and projection layer. Scalameta trees are source syntax; they are
  not the project's semantic model.
- `hybridScalametaFrontend` is an unpublished opt-in typed Term/Type frontend.
  It maps public Scalameta ASTs into the same core models and then lowers or
  matches in the caller's `Quotes` universe.
- `dottyInternal` contains unpublished exact-version `untpd` adapters and the
  narrow `ContextualMethodPeerBridge`. It is not a general raw-tree API.
- the aggregate and example projects publish no production artifacts.

Only a Scalameta parse failure may select the current parser inside the hybrid
route. Exact-compiler syntax rejection, unsupported mapped syntax, semantic
mapping failure, reflected target inspection failure, and lowering failure are
terminal. They never silently switch engines and widen the accepted language.

The two typed frontends aim for parity on deliberately overlapping admitted
Term and Type slices. Differential tests remain appropriate whenever both
routes claim a feature. This is not a lock-step promise that every future
feature must land in both routes simultaneously. Ordinary released/default
`qr`/`qq` and `tqr`/`tqq` remain current-Dotty; the Scalameta route remains
explicit, experimental, and remotely unpublished.

## Representation and ownership boundaries

`core` has no Scala compiler or Scalameta dependency. Its block and definition
models use project-owned binder identities; display spelling does not replace
semantic identity. Reflected captures remain owned by the caller's `Quotes`
universe, while exact `untpd` results remain compiler-version-coupled.

The compiler-free model is symbol-free. For future source-like definition and
class quasiquotes, symbols and owners that are derivable from syntax belong to
the typed backend's lowering plan. A Quotes backend may need to create a class
symbol before member symbols and definitions; a pre-typer `untpd` backend must
emit syntax without fabricating typed symbols.

```text
NO_PUBLIC_SYMBOL_QUASIQUOTE_FAMILY_CURRENTLY_PLANNED
TYPED_OWNED_DEFINITION_SYMBOL_SYNTHESIS = BACKEND_RESPONSIBILITY
NEUTRAL_CORE = SYMBOL_FREE
UNTYPED_PRE_TYPER_BACKEND = NO_TYPED_SYMBOL_FABRICATION
```

An advanced owner/definition-plan handle may eventually be justified by a
real consumer, but symmetric `sqr`/`sqq` symbol syntax is not currently
planned: symbols are compiler semantic entities, not source syntax.

## Cross-project boundary

AUXify's current narrow `@apply` path uses ordinary Scalameta `q`, `t`, and
`tparam` authoring and Quasiquotes `ContextualMethodPeerBridge` lowering to an
exact positioned `untpd.DefDef`. That is evidence for neutral source-like
authoring on one admitted integration path, not proof that every handler
should use Scalameta.

Macro-Paradise remains an exact compiler plugin. It owns plugin lifecycle,
placement, companion merge, insertion, rollback, and typing, and does not take
Quasiquotes or Scalameta as a core product dependency.

See [execution environments and AST representations](EXECUTION_ENVIRONMENTS_AND_AST_REPRESENTATIONS.md),
[Scalameta opt-in artifact topology](SCALAMETA_OPT_IN_ARTIFACT_TOPOLOGY.md), and
[why quasiquotes?](WHY_QUASIQUOTES.md).
