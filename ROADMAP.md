# Product roadmap

This experimental roadmap describes product capability rather than internal
delivery chronology.

## Current

- Preserve the typed integer/infix overlap decision: current-Dotty
  `qr`/`qq`, opt-in typed Scalameta `qr`/`qq`, programmatic `TermFrontend`, the
  neutral projector, and the current parser agree on the fixed no-hole
  arithmetic/comparison family across Scala 3.3.8, 3.8.4, and final 3.9.0.
  Keep direct typed Scalameta AST-to-reflection lowering and share the
  differential parity contract; do not route it through the narrower neutral
  projector or add a core-to-typed lowerer merely for symmetry.
- Preserve the representation boundary: public `Expr` and
  `quotes.reflect.Term` APIs are distinct from exact `tpd`/`untpd` internals,
  while Scalameta and the compiler-free core form a separate neutral axis.
  `dottyInternal` remains an unpublished exact backend, not a generic raw-tree
  toolkit.
- Preserve the production compiler-free `scala.meta.Term -> TermShape`
  projector for the accepted literal/infix/unary/tuple/conditional/name/select/
  one-list-Apply family, one typed Lambda1, bounded P0/P1/P2/P3 blocks, and a
  fully-qualified non-generic constructor with exactly one ordinary positional
  argument list. Constructor arguments reuse the existing recursive Term
  projection. Simple/import-relative constructors, constructor Type arguments,
  multiple/contextual lists, named/star arguments, anonymous templates, and
  every other unsupported Term shape fail closed. The projector retains one
  truthful root span, makes semantic copies rather than preserving raw-tree
  identity, and performs no name, class, overload, or symbol resolution.
- Preserve the package-private production exact backend for that same bounded
  family. `CoreTermShapeUntypedLowerer` accepts canonical signed decimal
  integer strings, the fixed ordinary operator set `+`, `-`, `*`, `/`, `%`,
  `==`, `!=`, `<`, `<=`, `>`, `>=`, direct identifiers, recursive selections,
  and exactly one ordinary positional Apply list. It validates direct names,
  recursively produces parser-equivalent source-free raw trees, and rejects
  placeholders, a direct Apply in function position, and every other core Term
  family. It is not a public Scalameta-to-Dotty bridge or a generic `TermShape`
  backend. Exact structural rewriting of existing raw trees remains a separate
  U experiment and is not absorbed by this N-to-D route.
- Preserve the richer package-private exact Term backend's bounded P2 closure:
  one already-admitted `LocalVal` block consumes its authoritative completed
  declared-Type sidecar before initializer sidecars, lowers the initializer in
  the incoming scope, installs the existing BinderId only for later block
  children, restores the incoming scope at exit, and supports both source-free
  and generated-origin parser-equivalent raw trees. Preserve the accepted
  bounded P3 closure in that same richer backend: one local identity method
  consumes authoritative parameter/result completed-Type sidecars, installs
  parameter and method binders in their distinct scopes, restores the incoming
  environment, and emits source-free or generated-origin parser-equivalent raw
  trees. The direct Core lowerer still rejects P2 and P3 because it has no
  authoritative completed-Type sidecars.

- Preserve one semantic architecture: source frontends project into the
  project-owned compiler-free model in `core`, followed by backend-specific
  lowering or reflected matching. Keep current-Dotty as the released/default
  reference route and Scalameta as explicit unpublished opt-in routes.
- Keep `core` compiler-free and independently consumable.
- Keep `frontend` compiler-version-coupled and test source parsing, matching,
  construction, diagnostics, and quoted lowering together.
- Keep the binary-crossed `neutralScalameta` experiment unpublished and
  isolated from compiler implementation, staging, and SemanticDB dependencies.
  Use direct Scalameta authoring until a genuinely reusable façade can delegate
  upstream macros without forwarding or duplicating them.
- Preserve bounded neutral reverse authoring through
  `ScalametaTermShapeAuthoring`: fresh `Position.None` Scalameta Terms for the
  accepted binder-free ordinary family, fully-qualified `new`, and binder-free
  P1 blocks, with exact project-shape round trips. Keep P2/P3 binders,
  interpolation, typed parentheses, and source-provenance reconstruction out of
  this authoring surface.
- Keep the compiler-coupled `hybridScalametaFrontend` experiment unpublished
  and side by side with the current engine. Its admitted typed Term, Type, and
  bounded Definition slices expand only through differential tests,
  exact-compiler validation, original reflected-hole/capture identity, and
  compiler-line dialect selection; do not switch the public default without a
  separate compatibility decision.
- Keep `dottyInternal` source visible but its artifact unpublished. Preserve the
  category-specific public exact-version `ScalametaTermUntypedBridge` for its
  documented direct non-binder and P0/P1 intersection, and the context-free
  sibling `ScalametaTypeUntypedBridge` for the recursive primitive,
  List/Option/Either, Tuple2/3-syntax, and Function1/2-syntax intersection.
  Keep Term and Type as distinct categories, keep Definition out of both, and
  do not generalize them into a raw-tree toolkit. Retain
  the definition-specific exact-version bridges for tightly coupled
  foreign-package peers. `ContextualMethodPeerBridge` admits the unchanged
  legacy `Show[A]` method and complete bounded `Add.Out` method.
  `SelfAbstractTypeMemberPeerBridge` admits only the coherent bounded
  abstract member. Preserve the delegated-forwarder, refined-Type-alias, and
  complete instance-factory bridges at their exact admitted shapes; none is a
  generic raw-tree API.
- Preserve Apache-2.0 POM and JAR metadata for intended `core` and `frontend`
  distributions.
- Expand structural term and type support through narrow, test-backed slices.
- Preserve the bounded reflected-Type composition slice at the complete
  constructor Type and source-owned local-definition parameter/result Type
  positions. `qr` accepts caller-owned `TypeRepr` directly so
  `TypeTree.tpe`, `TypeRepr.of[T]`, and `tqr` share one transport; preserve the
  compiler-free `QuasiTypeSplice` route and keep `scala.quoted` out of `core`.
- Preserve the bounded public sequence-Term construction slice: callers use
  `TermSequenceSplices.termSplice(Seq[q.reflect.Term])` with the explicit
  `..$args` marker in one ordinary Apply or supported one-list New argument
  list. Keep one repeated hole per list, exact order and original Term objects,
  and no matching, other ranks, additional clauses, vararg-star semantics,
  neutral projection, reparse, or generic owner repair.
- Preserve the first source-owned local `def` statement in `qr`: one literal
  identity-shaped method with one ordinary parameter, complete reflected Types,
  and a binder-resolved following result, with backend-created symbols and
  owners. Keep external typed `DefDef`
  statement splices deferred until an explicit reownership contract exists;
  do not add direct `Symbol` splicing.
- Preserve the typed class implementation result: public reflection on
  Scala 3.3.8, 3.8.4, and final 3.9.0 can synthesize a local parameterless class,
  one non-overloaded override, primary-constructor invocation, and unchanged
  literal/local caller-Term and invocation-argument capture without raw Symbol
  interpolation. The package-private `frontend` plan and public-reflection
  lowerer now implement that exact bounded route, including dynamic legal
  class/method/parameter names, generated binder identity, overload exclusion,
  detached-owner rejection, and owned-definition capture rejection. Preserve
  `S1`—no public Symbol quasiquote family—and add no public class syntax. N1 and
  N4 remain incomplete, external owned definitions still need a rebuild/
  reownership contract, and the next rotated gate belongs to neutral/core work
  rather than another typed/public slice. See
  [typed class, symbol, and owner feasibility](docs/TYPED_CLASS_SYMBOL_OWNER_FEASIBILITY.md),
  which now records both the oracle and the internal implementation boundary.
- Preserve the bounded construction-only selected-member name hole: a public
  validated decoded-name value, one explicit receiver selection-name position,
  unique accessible `Select.unique` lowering, unchanged fixed-name matching,
  and Scalameta opt-in parity on the overlapping construction slice.
- Preserve the bounded compiler-free one-ordinary-parameter definition core,
  core-only public identity-method first use, and unpublished exact backend
  without implying a general source adapter or placement policy.
- Preserve the accepted same-spelling current-Dotty Definition pattern
  direction: static exact-one `dqq` retains
  `SingleParameterDefinitionPattern`, static structural exact-two `dqq` uses
  scalable `DefinitionPatternExtractor`, and dynamic/non-static calls retain
  the historical exact-one fallback. Do not introduce `dqq2`/`dqq3`/`dqq4` or
  arity-numbered public pattern carriers. Keep the current exact-two `dqr`
  slice bounded to one ordinary clause, standalone `Int`/`String`/`Boolean`
  types, and a literal body selecting either binder under the unchanged
  variadic signature.
- Preserve typed-Scalameta parity for that same bounded Definition slice:
  static exact-one and exact-two templates select their corresponding pattern
  carriers, dynamic/non-static matching retains the exact-one fallback, and
  successful matches expose the caller's original RHS. This is frontend parity,
  not a general neutral Definition projection or exact-tree placement API.
- Maintain end-to-end recursive `List`/`Option` and binary `Either` support
  without turning fixed constructor admission into a general type resolver.
- Preserve a deterministic clean aggregate build and external-package examples.
- Preserve the bounded structural Scalameta-to-validated-IR projection and the
  exact backend-only reverse projection without print/reparse or fabricated
  source provenance.
- Measure whether public Scalameta parsing materially reduces grammar
  maintenance while current-Dotty remains the exact compiler oracle and only
  Scalameta parse failure may use the current parser as fallback.

## Composable quasiquotes

The following work advances in stages. The complete constructor-Type slice is
implemented; the other interpolation and import forms remain planned unless
the text explicitly says otherwise.

### Reflected Types in Term construction

1. Caller-owned `q.reflect.TypeRepr` is its own typed-Term interpolation
   category. Its admitted positions are the complete constructor Type in
   `new $typeValue(arg)` and the complete parameter/result Types in the bounded
   source-owned local-definition form.
2. A value returned by `tqr` works directly in that position.
   `TypeTree.tpe` and `TypeRepr.of[T]` use the same transport. Literal canonical
   globally selected class terminals such as `tqr"java.lang.StringBuilder"`
   resolve through exact typed identity. Convenience for
   passing `Type[T]` or `TypeTree` directly may be considered only after the
   `TypeRepr` contract is stable.
3. Treat other Type positions as later slices: applied or dynamic
   constructors, method Type applications, refinements, and sequence Type
   splices each need their own admission and failure rules.

The existing `QuasiTypeSplice(ConstructedType)` remains the compiler-free
route. Reflected Types will not be normalized through that bounded model.

### Definitions and statements

1. Preserve the implemented first source-owned local `def` inside one `qr`
   block. That block owns symbol creation, binders, owners, and the following
   reference to the method; broader bodies and statement topology remain later
   slices.
2. Consider a separately constructed `DefDef` or Definition statement splice
   only after owner validation and complete rebuild/reownership semantics are
   explicit.
3. Add repeated or sequence statement splices only after the single-statement
   ownership model is proven.

Direct `Symbol` splicing is not planned. In particular,
`$definition(...)` must not become implicit shorthand for
`Ref(definition.symbol)(...)`; a definition statement and a method reference
are different source categories.

### Typed Scalameta Definitions

Typed Scalameta `dqr`/`dqq` are implemented in the unpublished opt-in hybrid
frontend for the exact current-Dotty one-ordinary-parameter and exact-two
identity-Definition overlap. Construction delegates to the corresponding typed
owner/binder lowerer. Matching uses `SingleParameterDefinitionPattern` for
static exact-one syntax and `DefinitionPatternExtractor` for static exact-two
syntax, with the same dynamic exact-one fallback; successful matching returns
the caller's original reflected RHS. Accepted single-parameter
construction includes structured Tuple2/Tuple3, Function1/Function2, and
current-policy nested `List`/`Option`/`Either` combinations through complete
`TypeNormalForm`; it does not widen public `CompletedType`. Exact-two remains
limited to one ordinary clause, standalone `Int`/`String`/`Boolean` Types, and
a body selecting either declared binder. These APIs are not aliases
for neutral upstream Scalameta `q` definition AST authoring, and they are not
the exact pre-typer bridge in `dottyInternal`. Parity between typed frontends
remains required only for their overlapping advertised slices, not in lock-step.

### Product-level Definition ladder

Definition work proceeds by semantic ownership rather than syntax breadth:

1. immutable typed value;
2. parameterless ordinary method;
3. one ordinary-parameter method;
4. exact-two ordinary-parameter method;
5. simple Type alias;
6. broader clauses, Types, and bodies;
7. class, trait, and object construction only after a reusable owner/member model exists.

A future neutral API may take the conceptual shape
`ScalametaDefinitionProjection.project(stat)`, but that spelling is illustrative,
not a locked public name. The next implementation step is a reusable neutral
Definition projection model; specialized contextual-method, Type-alias, and
instance-factory projectors do not substitute for it.

The existing neutral Term and Type projectors can each compose with an existing
internal exact lowerer for their admitted non-binder families. They are ready
for separate bounded public façade and diagnostic-design tasks. No such public
API is declared here, and a general Scalameta Definition-to-exact-tree bridge
waits on the reusable neutral Definition projection step.

Publication policy remains a separate successor task: it must decide stable
coordinates, supported compiler lanes, compatibility evidence, and release
gates without changing semantic admission merely to make publication easier.

### Additive import façade

The accepted additive umbrella import for the current-Dotty route is:

```scala
import quasiquotes.Quasiquotes.{qr, qq, tqr, tqq, dqr, dqq}
```

and, for the typed Scalameta route:

```scala
import quasiquotes.scalameta.Quasiquotes.{qr, qq, tqr, tqq, dqr, dqq}
```

The typed Definition surface is included in that façade too. Both objects
are direct exports over the existing hosts, preserve exact transparent-inline
and ranked typing, and add no semantic wrapper. Current public package imports
remain supported; no deep package move or deprecation is selected. At that
umbrella-facade task's historical checkpoint, the exact accepted API delta was
additive: standard 676 to 677 rows and hybrid 42 to 43 search rows, with one
object addition and zero removals in each inventory. The current programme
inventory is the current 679-row / 661-group standard surface and the
unchanged 43-row hybrid surface recorded above.

## North-star source-like generation

The durable [north-star checkpoint document](docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md)
keeps the manual reflection baseline, conceptual future source-like shape,
missing capabilities, and completion criterion separate for every example.
All conceptual spellings remain non-current notation rather than selected
syntax.

| Checkpoint | Current status | Enabling gap |
| --- | --- | --- |
| N1 generic subclass with override | `CURRENT_MANUAL_BASELINE_PROVED`, `BOUNDED_INTERNAL_PLAN_IMPLEMENTED`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | supported class syntax and broader body composition beyond the one-override internal route |
| N2 runtime-length dynamic Type application | `CURRENT_MANUAL_BASELINE_PROVED`, `PARTIALLY_COVERED_BY_CURRENT_QUASIQUOTES`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | constructor-position Type hole and sequence Type splice |
| N3 generated Type refinement members | `CURRENT_MANUAL_BASELINE_PROVED`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | refinement/type-member model and sequence definition splice |
| N4 anonymous implementation with calculated definitions | `CURRENT_MANUAL_BASELINE_PROVED`, `BOUNDED_INTERNAL_PLAN_IMPLEMENTED`, `PARTIALLY_COVERED_BY_CURRENT_QUASIQUOTES`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | anonymous-class syntax, broader definitions, sequence splices, and composition over the bounded class-owner plan |
| N5 dynamic `new T(..args)` for an existing type | `CURRENT_MANUAL_BASELINE_PROVED`, `PARTIALLY_COVERED_BY_CURRENT_QUASIQUOTES`, `COMPLETE_CONSTRUCTOR_TYPE_SPLICE_IMPLEMENTED`, `BOUNDED_SEQUENCE_TERM_CONSTRUCTION_IMPLEMENTED`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | broader constructor/argument-clause and coercion policy |

None of N1-N5 is `CHECKPOINT_COMPLETE`.

The bounded explicit-receiver dynamic selected-member construction gap, the
first N5 complete constructor-Type and bounded sequence-Term splices, and the first source-owned local
identity-method block are implemented. Bare identifiers,
overload resolution, dynamic infix syntax, dynamic name matching, sequence
matching and other sequence ranks, broader Type positions, and broader
definition/class support remain independent later work. The
[typed sequence Term splice contract](docs/TYPED_SEQUENCE_TERM_SPLICE_DESIGN.md)
ships `..$args` plus a dedicated caller-universe carrier for one bounded
Apply/one-list-New construction gate. The
bounded contextual-method and self abstract-Type-member bridges and their
foreign-package consumer proofs are complete. Type-alias, instance-factory,
delegated-forwarding, and anonymous-implementation work remain separate
peer-oriented lanes; the self abstract-Type-member bridge did not widen them.
External `DefDef` statement splicing is not selected merely for symmetry.
After the two completed typed/public rotation slots, the selected neutral/core
gate expands `ScalametaTermProjection` from the original integer/infix family
to direct identifiers, selections, and one ordinary positional Apply list.
For example, `q"obj.f(1 + 2, 3)"` maps directly to recursive project-owned
`TermShape` without binders, Type sidecars, compiler lookup, or a public
frontend switch. The exact backend intentionally remains narrower: only the
integer/infix core family lowers to source-free, span-free, symbol-free
`untpd.Tree`; Identifier, Select, and Apply fail at its existing unsupported-
shape boundary. Direct `Typer.typedExpr` on a `NoSpan` `untpd.InfixOp` is not
viable because Dotty's infix desugaring reads operand and operator spans;
three-line verification therefore uses a test-only source-free
`Apply(Select(...), ...)` typing shell after separately proving the production
tree's parser-equivalent raw shape.

Symbols are compiler semantic entities, not source syntax. The feasibility
gate selects `S1`: no public Symbol quasiquote family. The neutral core remains
symbol-free;
typed owned-definition symbol synthesis belongs to the typed backend, while an
untyped pre-typer backend must not fabricate typed symbols. A future advanced
owner/definition-plan handle needs concrete consumer evidence. Construction
does not decide whether later Symbol matching/extraction needs a separate
semantic design.

## Ongoing public-project hygiene

- Reconfirm the Apache-2.0 provenance/attribution audit remains current.
- Keep security, support, contribution, and community-policy wording current
  with the experimental research status.
- Run independent source/history residual scanning and a human public-content
  audit for each visibility candidate.
- Keep the documented experimental early-semver and compiler-line policy
  aligned with the reviewed public API inventory.
- Reconsider whether a private security-reporting channel is warranted as the
  project and its support commitments evolve; none is currently promised.

## Before a later artifact release

- Select the next version from reviewed API and compatibility evidence and
  reconfirm the Central Portal publication path.
- Use explicitly approved public developer and signing identity inputs.
- Complete real-key signing, provenance, POM, source/Javadoc, and
  reproducibility checks.
- Validate clean coordinate-only consumers on every promised Scala/JDK lane.
- Publish only separately approved coordinates; keep the
  forward-probe frontend, aggregate root, examples, `neutralScalameta`,
  `hybridScalametaFrontend`, and `dottyInternal` unpublished.

Later releases remain separate decisions. Development continues through
bounded language, usability, compatibility, and backend improvements without
assuming publication.
