# Architecture

The durable language pipeline is:

```text
source frontend(s)
    -> project-owned compiler-free semantic model in core
    -> backend-specific lowering / reflected matching
```

The project owns one compiler-free semantic model. The current-Dotty frontend
is the released default and the reference oracle; the typed Scalameta route is
an explicit, unpublished opt-in. The two typed routes must agree wherever they
both advertise support, but they do not have to acquire every feature in
lock-step. In the hybrid route, only a Scalameta parse failure may fall back to
the current parser.

There are multiple source-facing routes, but there are not multiple semantic
quasiquote engines. `core` owns the compiler-free Term and Type normal forms,
templates, patterns, binder identities, structural construction and matching
rules, and neutral diagnostics. Frontends must project into those shared
models rather than inventing frontend-local equality or binding semantics.

## Modules and dependency direction

```text
frontend -------------------------> core
neutralScalameta -----------------> core + Scalameta
dottyInternal --------------------> neutralScalameta + exact Scala compiler
hybridScalametaFrontend ----------> frontend + neutralScalameta
publicApiExamples ----------------> frontend
publicCoreExamples ---------------> core
```

Every arrow means “depends on.” Consequently `dottyInternal` receives `core`
and Scalameta transitively through `neutralScalameta`, while
`hybridScalametaFrontend` has both the current typed frontend and the neutral
Scalameta projection available. `frontend` never depends on `dottyInternal`.

The production neutral Term route and its narrower exact continuation are:

```text
scala.meta.Term (bounded literals / names / select / Apply / infix / unary
  / tuple / if / Lambda1 / P0-P3 block families)
  -> ScalametaTermProjection
  -> core TermShape
  -> package-private CoreTermShapeUntypedLowerer (non-binder + P0/P1 overlap)
  -> source-free untpd.Tree
```

No arrow in this route performs name resolution, overload resolution, typing,
precedence parsing, or source/provenance reconstruction. Precedence is already
encoded by recursive `TermShape.Infix` structure.

- `frontend` is the released/default exact-compiler route. It owns parsing,
  quoted reflection, public `qr`/`qq` and `tqr`/`tqq`, and compiler-line
  lowering. It is also the first-class reference implementation and oracle.
- `neutralScalameta` is an unpublished compiler-free Scalameta 4.17.3 AST
  boundary and projection layer. Scalameta trees are source syntax; they are
  not the project's semantic model. Its production Term projection admits
  semantic Int/String/Boolean literals, recursive ordinary binary infix and
  unary nodes, tuples, explicit three-branch conditionals, direct identifiers,
  direct selections, exactly one ordinary positional Apply argument list, one
  explicitly typed Lambda1, transparent P0/binder-free P1 blocks, one bounded
  typed local-val P2 block, and one bounded source-owned local identity-method
  P3 block into core `TermShape`. Unsupported and broader binder/statement
  shapes fail closed.
- `hybridScalametaFrontend` is an unpublished opt-in typed Term/Type frontend.
  It reuses project-owned templates, patterns, matching, and Type models where
  applicable. Term construction currently lowers Scalameta ASTs directly in
  the caller's `Quotes` universe; it does not route through the narrower
  neutral `TermShape` projector.
- `dottyInternal` contains unpublished exact-version `untpd` adapters and the
  narrow `ContextualMethodPeerBridge`, `SelfAbstractTypeMemberPeerBridge`, and
  `DelegatedForwardingMethodPeerBridge`, plus the bounded
  `AuxTypeAliasPeerBridge`. Its package-private
  `CoreTermShapeUntypedLowerer` consumes the accepted neutral non-binder family
  (Int/String/Boolean literals, infix, unary, tuple, conditional,
  Identifier/Select/one-list Apply) plus transparent P0 and binder-free P1
  blocks, and emits source-free raw syntax. A direct Apply in function
  position is rejected as multiple lists, while Apply remains valid in
  argument and qualifier positions. Lambda1 and P2 are not admitted by this
  direct lowerer; P2 also remains outside the richer exact backend. This is not
  a general raw-tree API and does not perform U-style identity-preserving
  rewriting of existing raw trees.
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
explicit, experimental, and remotely unpublished today. The explicit
expanded-release mode can stage the selected `0.3.0` Scalameta and exact
backend candidate artifacts without changing this semantic ownership model.

For the accepted no-hole non-binder overlap, shared differential behavior
is the consolidation boundary. The neutral projector is a compiler-free
source-AST validator with a narrower topology and source-span contract; the
typed route owns active-`Quotes` lowering, reflected holes, member selection,
typed failures, and parse-only fallback. No suitable project-owned
`TermShape -> q.reflect.Term` lowerer currently exists, and adding one solely
to remove two small AST cases would introduce a third contract rather than
remove semantic duplication.

## Representation and ownership boundaries

`core` has no Scala compiler or Scalameta dependency. Its block and definition
models use project-owned binder identities; display spelling does not replace
semantic identity. Reflected captures remain owned by the caller's `Quotes`
universe, while exact `untpd` results remain compiler-version-coupled.

The compiler-free model is symbol-free, and no public symbol-quasiquote family
is currently planned. For future source-like definition and class
quasiquotes, symbols and owners that are derivable from syntax belong to the
typed backend's lowering plan. A Quotes backend may need to create a class
symbol before member symbols and definitions; a pre-typer `untpd` backend must
emit syntax without fabricating typed symbols.

### Composable reflected types and statements

The first reflected typed-Term construction slice is implemented around the
caller's active `Quotes` universe. A complete constructor-position hole accepts
`q.reflect.TypeRepr` directly; `TypeTree.tpe`, `TypeRepr.of[T]`, and a `tqr`
result therefore enter one transport without `Any`, serialization, or a
second public carrier. The compiler-free placeholder model may distinguish a
generic reflected-Type payload internally, while `core` does not import
`scala.quoted`. Existing `QuasiTypeSplice` remains the compiler-free
`ConstructedType` route and is not replaced.

The admitted positions are the complete constructor Type in
`new $typeValue(arg)` with one ordinary argument list and the complete
parameter/result Types of the bounded source-owned local method described
below. Ascriptions, method Type application, applied-Type constructors, other
definition Types, and variadic Type arguments remain independent slices.
Direct `TypeRepr` transport makes `tqr` to `qr` stacking a tested current
behavior.
The typed Scalameta route implements the same overlap and treats reflected-Type
admission or lowering failure as terminal rather than a fallback trigger.

Definition composition has two ownership classes. A source-owned local
`def` written inside `qr` is implemented for one ordinary parameter, a body
that is exactly that parameter reference, and one following result expression.
`core` records distinct method and parameter `BinderId`s; the typed backend
allocates a fresh method under the lowering owner, uses the callback-owned
parameter for the RHS, and binds the following method reference by identity.
An already typed external `DefDef` is not a detached syntax node: insertion
needs an explicit owner/reownership contract and must fail closed until that
contract exists. Splicing a `Symbol` as shorthand for `Ref(symbol)` is not
planned.

An advanced owner/definition-plan handle may eventually be justified by a
real consumer, but symmetric `sqr`/`sqq` symbol syntax is not currently
planned: symbols are compiler semantic entities, not source syntax.

## Cross-project boundary

The live product builds have this consumer topology; arrows again mean build
dependency or data flow, not semantic ownership:

```text
ordinary Quotes users ---> frontend
                      `--> hybridScalametaFrontend (explicit opt-in)

AUXify handlers ---> Scalameta directly + dottyInternal     [build dependency]
                 ---> neutralScalameta transitively         [build dependency]
                 ---> exact untpd result                    [data flow]
                 ---> Macro-Paradise placement and lifecycle [data flow]

Macro-Paradise ---> no Quasiquotes or Scalameta product dependency
```

The neutral source pipeline used by the admitted exact peer bridges is:

```text
Scalameta q/t -> scala.meta AST
  -> neutralScalameta validated projection
  -> core IR
  -> exact backend
```

Plain Scalameta `q`/`t` construction and matching stops at `scala.meta` AST and
does not require `neutralScalameta`; that module exists for bounded validated
projection into this project's shared model.

The reusable neutral Term route is now:

```text
scala.meta Int/String/Boolean literals / ApplyInfix / unary / tuple / if
  / Term.Name / Term.Select / one ordinary Term.Apply argument list
  / one typed Lambda1 / transparent P0 and bounded P1/P2/P3 blocks
  -> ScalametaTermProjection
  -> core TermShape
```

It preserves only a truthful root source span and performs no rendering,
reparse, typing, symbol lookup, overload resolution, or fallback. The recursive
result is a semantic copy; it does not preserve Scalameta child identity or raw
Dotty subtree identity and adds no opaque raw sidecar. Nested Apply lists, Type
application, contextual clauses, named/star arguments, `new`, and broader
statement/binder forms remain outside the neutral contract. The direct
exact backend accepts the non-binder family above plus transparent P0 and
binder-free P1 blocks; a direct Apply in function position is rejected as a
second argument list, while Apply remains valid in argument and qualifier
positions. Lambda1, P2, and P3 do not cross this direct edge; P2 and P3 remain
closed in the richer exact path. The typed Scalameta frontend remains direct
and is not refactored through this projector.

AUXify's current narrow `@apply` path uses ordinary Scalameta `q`, `t`, and
`tparam` authoring and Quasiquotes `ContextualMethodPeerBridge` lowering to an
exact positioned `untpd.DefDef`. That is evidence for neutral source-like
authoring on one admitted integration path, not proof that every handler
should use Scalameta.

The bounded `@self` member path uses a typed Scalameta `Decl.Type` and
`SelfAbstractTypeMemberPeerBridge` to obtain one positioned `untpd.TypeDef`.
The prepared self alias remains peer-owned external syntax: Quasiquotes
validates its repeated uses but does not allocate a binder or create the alias.

The bounded delegated-forwarder path accepts one already-authored Scalameta
`Defn.Def`, validates three distinct declaration roles and their references,
and uses `DelegatedForwardingMethodPeerBridge` to obtain a positioned
`untpd.DefDef`. AUXify still owns method derivation, and Macro-Paradise still
owns source inspection, companion lifecycle, placement, conflict policy, and
rollback. This operation is not a general method or Term bridge.

The bounded AUXify-039 alias path accepts one already-authored Scalameta
`Defn.Type` plus explicit alias, three parameter, three bound, target, and
refinement-member expectations. `ScalametaAuxTypeAliasProjection` produces the
three-binder `AuxTypeAliasPlan`; one private identity-preserving adapter copies
those binder identities into the accepted U001 input, and
`AuxTypeAliasPeerBridge` returns a positioned 18-node `untpd.TypeDef`. AUXify
owns fresh-name derivation and authoring. Macro-Paradise retains all lifecycle,
admission, companion, placement, conflict, rollback, and typing policy. See the
[AUXify-039 bridge contract](AUXIFY039_TYPE_ALIAS_PEER_BRIDGE.md).

Macro-Paradise remains an exact compiler plugin. It owns plugin lifecycle,
placement, companion merge, insertion, rollback, and typing, and does not take
Quasiquotes or Scalameta as a core product dependency.

See [execution environments and AST representations](EXECUTION_ENVIRONMENTS_AND_AST_REPRESENTATIONS.md),
[Scalameta opt-in artifact topology](SCALAMETA_OPT_IN_ARTIFACT_TOPOLOGY.md), and
[why quasiquotes?](WHY_QUASIQUOTES.md). Future manual-reflection replacement
goals are tracked separately as [north-star quasiquote examples](NORTH_STAR_QUASIQUOTE_EXAMPLES.md);
they do not widen the current architecture or syntax boundary.
