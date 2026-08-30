# Product roadmap

This experimental roadmap describes product capability rather than internal
delivery chronology.

## Current

- Preserve the Phase-142 typed integer/infix overlap decision: current-Dotty
  `qr`/`qq`, opt-in typed Scalameta `qr`/`qq`, programmatic `TermFrontend`, the
  neutral projector, and the current parser agree on the fixed no-hole
  arithmetic/comparison family across Scala 3.3.8, 3.8.4, and 3.9.0-RC1.
  Keep direct typed Scalameta AST-to-reflection lowering and share the
  differential parity contract; do not route it through the narrower neutral
  projector or add a core-to-typed lowerer merely for symmetry. The next
  rotated semantic track is peer work, with AUXify input 043 selected for a
  separate bounded forwarding-method bridge design from fresh prerequisite
  evidence.
- Preserve the Phase-131 representation boundary: public `Expr` and
  `quotes.reflect.Term` APIs are distinct from exact `tpd`/`untpd` internals,
  while Scalameta and the compiler-free core form a separate neutral axis.
  `dottyInternal` remains an unpublished exact backend, not a generic raw-tree
  toolkit.
- Preserve the production compiler-free `scala.meta.Term -> TermShape`
  projector for exactly recursive semantic `Lit.Int` values and ordinary
  one-RHS binary infix nodes. It retains truthful root offsets and fails closed
  for every other Term shape. The broader Phase-131 select/apply/one-list
  `new`/identifier helper remains test-only feasibility evidence rather than
  production support.
- Preserve the package-private production exact backend for that same bounded
  family. `CoreTermShapeUntypedLowerer` accepts canonical signed decimal
  integer strings and the fixed ordinary operator set `+`, `-`, `*`, `/`, `%`,
  `==`, `!=`, `<`, `<=`, `>`, `>=`; it recursively produces parser-equivalent
  source-free `untpd.Number`/`untpd.InfixOp` trees and rejects every other core
  Term family. It is not a public Scalameta-to-Dotty bridge or a generic
  `TermShape` backend.

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
- Keep the compiler-coupled `hybridScalametaFrontend` experiment unpublished
  and side by side with the current term engine. Expand its admitted `qr`/`qq`
  slice only through differential tests, exact-compiler validation, original
  reflected-hole/capture identity, and compiler-line dialect selection; do not
  switch the public default without a separate compatibility decision.
- Keep `dottyInternal` source visible but its artifact unpublished. Retain only
  definition-specific exact-version bridges for tightly coupled foreign-package
  peers. `ContextualMethodPeerBridge` admits the unchanged legacy `Show[A]`
  method and complete AUXify-037 `Add.Out` method.
  `SelfAbstractTypeMemberPeerBridge` admits only the coherent AUXify-046
  bounded abstract member; neither is a generic raw-tree API.
- Preserve Apache-2.0 POM and JAR metadata for intended `core` and `frontend`
  distributions.
- Expand structural term and type support through narrow, test-backed slices.
- Preserve the bounded reflected-Type composition slice at the complete
  constructor Type and source-owned local-definition parameter/result Type
  positions. `qr` accepts caller-owned `TypeRepr` directly so
  `TypeTree.tpe`, `TypeRepr.of[T]`, and `tqr` share one transport; preserve the
  compiler-free `QuasiTypeSplice` route and keep `scala.quoted` out of `core`.
- Preserve the first source-owned local `def` statement in `qr`: one literal
  identity-shaped method with one ordinary parameter, complete reflected Types,
  and a binder-resolved following result, with backend-created symbols and
  owners. Keep external typed `DefDef`
  statement splices deferred until an explicit reownership contract exists;
  do not add direct `Symbol` splicing.
- Preserve the typed class implementation result: public reflection on
  Scala 3.3.8, 3.8.4, and 3.9.0-RC1 can synthesize a local parameterless class,
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

Typed Scalameta `dqr`/`dqq` are a future candidate after a shared typed
Definition slice exists. They must reuse the same compiler-free Definition
semantics and typed-backend ownership plan as the current-Dotty route. They
are not aliases for neutral upstream Scalameta `q` definition AST authoring,
and they are not the exact pre-typer bridge in `dottyInternal`. Parity between
typed frontends remains required only for their overlapping advertised slices,
not in lock-step.

### Additive import façade

The selected future ergonomics are additive umbrella imports:

```scala
import quasiquotes.Quasiquotes.{qr, qq, tqr, tqq, dqr, dqq}
```

and, for the typed Scalameta route:

```scala
import quasiquotes.scalameta.Quasiquotes.{qr, qq, tqr, tqq}
```

The Scalameta façade may add `dqr`/`dqq` only after that typed Definition
surface exists. Current public package imports will remain supported; no deep
package move or deprecation is selected. Separate domain façades remain a
fallback, not the primary direction.

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
| N5 dynamic `new T(..args)` for an existing type | `CURRENT_MANUAL_BASELINE_PROVED`, `PARTIALLY_COVERED_BY_CURRENT_QUASIQUOTES`, `COMPLETE_CONSTRUCTOR_TYPE_SPLICE_IMPLEMENTED`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | sequence Term splice and broader constructor/argument policy |

None of N1-N5 is `CHECKPOINT_COMPLETE`.

The bounded explicit-receiver dynamic selected-member construction gap, the
first N5 complete constructor-Type splice, and the first source-owned local
identity-method block are implemented. Bare identifiers,
overload resolution, dynamic infix syntax, dynamic name matching, sequence
splices, broader Type positions, and broader definition/class support remain
independent later work. The bounded AUXify-037 and AUXify-046 bridges and their
foreign-package consumer proofs are complete. Inputs 039, 041, 043, and 045
remain separate peer-oriented lanes; 046 did not implement or widen them.
External `DefDef` statement splicing is not selected merely for symmetry.
After the two completed typed/public rotation slots, the selected neutral/core
gate is implemented: `ScalametaTermProjection` maps the bounded integer/infix
family, including `q"1 + 1"`, directly to project-owned `TermShape`. It does not
admit binders, type sidecars, arbitrary Terms, or a public frontend switch. The
following exact-backend gate is also implemented: the same bounded core family
lowers directly to source-free, span-free, symbol-free `untpd.Tree` without
parsing or generated provenance. Direct `Typer.typedExpr` on a `NoSpan`
`untpd.InfixOp` is not viable because Dotty's infix desugaring reads operand and
operator spans; three-line verification therefore uses a test-only source-free
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
