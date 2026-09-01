# Experimental AUXify-039 Type-alias peer bridge

`quasiquotes.definitions.dotty.AuxTypeAliasPeerBridge` is an exact-Scala-version
entry point in the remotely unpublished `dottyInternal` artifact. It accepts
one public Scalameta `Defn.Type` in the bounded AUXify-039 family:

```scala
type Aux[N <: Nat, M <: Nat, Out0 <: Nat] =
  Add[N, M] { type Out = Out0 }
```

The alias, all three parameter names, all three upper-bound names, applied
target, refinement member, and generated virtual-source name are explicit
inputs. Coherent renamed forms are admitted; arbitrary arity, lower or compound
bounds, qualified/applied bounds, qualified targets, reordered references,
multiple refinements, abstract or parameterized refinement members, and
arbitrary aliases are not.

The implementation deliberately composes existing bounded layers:

```text
scala.meta.Defn.Type
  -> ScalametaAuxTypeAliasProjection
  -> AuxTypeAliasPlan with three BinderIds
  -> private identity-preserving plan/input adapter
  -> AuxTypeAliasUntypedLoweringInput validation
  -> AuxTypeAliasGeneratedOriginAdapter
  -> positioned untpd.TypeDef
```

The private adapter copies every declaration and reference `BinderId` and
display spelling verbatim. It does not allocate IDs, infer roles from names,
render or reparse source, or bypass the exact lowerer's validation. Target
arguments retain binder roles one and two in order; the refinement RHS retains
role three.

On success, `lower` exposes only the positioned `untpd.TypeDef`, its
deterministic generated source, and the effective virtual-source name. The
generated tree has the admitted 18 nonempty-node topology. Every node has the
same generated source, a contained deterministic span, `NoSymbol`, and no
`TypedSplice` before ordinary typing. The input Scalameta position remains
projection evidence; it is not falsely copied onto newly generated raw nodes.

Failures retain their first owned boundary. Scalameta structure, expectation,
bound, and binder/reference rejections retain neutral projection codes. An
impossible projected-plan seam becomes `INTERNAL_INVARIANT_FAILED`, while exact
raw and generated-origin failures retain their U-owned categories. Invalid
virtual-source names are rejected as `INVALID_VIRTUAL_SOURCE_NAME` before
generated-origin lowering. There is no permissive fallback.

The ownership split is narrow:

- AUXify inspects its admitted source shape, derives fresh semantic names, and
  authors the `Defn.Type`;
- Quasiquotes validates, projects, adapts, and lowers that authored alias;
- Macro-Paradise continues to own annotation lifecycle, target admission,
  companion creation/merge, placement, conflicts, rollback, and ordinary
  typing.

Product fixtures cover Scala 3.3.8, 3.8.4, and 3.9.0-RC1. A disposable copy of
the current AUXify checkout consumes the Scala-3.8.4 candidate jars directly
and passes canonical, fresh-renamed, controlled-failure, and real
companion-placement checks. No Maven publication or real peer-repository
mutation is part of that proof.
