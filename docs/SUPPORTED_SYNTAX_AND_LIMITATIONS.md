# Supported syntax and limitations

The implementation is a structural research subset, not a complete Scala 3
quasiquote system.

Currently exercised areas include:

- identifiers, selections, applications, typed terms, blocks, tuples, unary
  operations, and standard string interpolation in bounded structural forms;
- ordinary lambdas with exactly one explicitly typed parameter, with scoped
  binder identity, alpha-aware construction/matching, and complete-body holes;
- a compiler-free public identity-method constructor with exactly one explicitly
  named and typed ordinary parameter, plus a richer package-private definition
  shape with scoped references and alpha-aware template/completion semantics;
- type identifiers, selections, applications, tuples, functions, wildcards,
  unions/intersections, annotations, refinements, and selected bounds/match
  forms;
- term and type holes with collision-safe rewriting and repeated-hole checks;
- compiler-free term/type/definition templates and completed values;
- bounded public contextual-method and single-ordinary-parameter construction
  contracts;
- source spans, diagnostic anchors, and exact-version lowering adapters;
- recursively nested `List` and `Option` types plus binary `Either`, with
  structural argument order, repeated type holes, construction, quoted
  lowering/inspection, typed ascriptions, and scoped `Type[t]` evidence.

Important limitations:

- no claim of complete Scala grammar or compiler-tree coverage;
- no stable raw-tree public API;
- no general owner/symbol repair or arbitrary generated-definition placement;
- Lambda1 excludes inferred or multiple parameters, nested/context/pattern
  lambdas, binder-name holes, local definitions, and general blocks;
- compiler-internal behavior is exact-version-sensitive;
- public definition construction is intentionally narrow;
- the one-parameter method surface has no public source interpolator, located
  parameter-span carrier, arithmetic/literal/general-expression body builder,
  or exact untyped/generated-origin backend yet;
- interpolation and type support expands incrementally, so unsupported shapes
  return explicit errors rather than falling back to unchecked trees;
- ordinary quoted standard-`s` interpolation has a bounded exact internal
  backend with canonical escaping and generated-origin spans; `raw`, `f`,
  custom interpolators, and triple-quoted `s` remain unsupported;
- APIs and rendered forms remain experimental and may change;
- applied constructor admission is fixed to `List` (arity 1), `Option`
  (arity 1), and `Either` (arity 2); arbitrary or selected constructors,
constructor holes, higher-kinded types, aliases, semantic name resolution,
subtyping, and compiler equality are not supported.

## Binder-aware Lambda1

The admitted lambda form is exactly one ordinary explicitly typed parameter:

```scala
(x: Int) => x
(x: Int) => x + 1
(x: Int) => f(x)
(x: Int) => if x > 0 then x else 0
```

The parameter declaration, references bound by it, and free references are
distinct structural roles. Consequently `(x: Int) => x` and `(y: Int) => y`
compare alpha-equally, while `(x: Int) => freeX` does not compare equal to an
identity lambda merely because `freeX` has similar text. Parameter type stays
part of structural equality.

`qr` builds a genuine quoted lambda. A spliced external term retains its
original resolved identity and is not captured when its source name matches
the new parameter display name. Splices containing local `val`, `def`, or
class definitions are rejected because this surface does not provide general
owner migration.

Pattern body holes return the original reflected target term, including its
compiler symbols. A captured subtree that refers to a lambda parameter remains
scope- and owner-sensitive; it is not a detached tree that can safely be moved
outside its original lambda. Repeated holes compare bound references relative
to their corresponding ambient scopes and preserve free-symbol identity.

The unpublished exact internal backend lowers this same bounded Lambda1 shape
both source-free and with generated-origin positions. It resolves bound
references through project binder identity and consumes the completed
parameter-type sidecar. Nested lambdas and broader lambda/block syntax remain
outside that internal contract and fail closed.

## Single ordinary-parameter definitions

The public compiler-free first-use surface admits the identity-like subset:

```scala
def id(x: Int): Int = x
def keep(x: String): String = x
```

Call `CompletedTerm.definitionParameterReference("x")` and pass the result to
`DefinitionConstruction.singleParameterMethod`. A plain
`CompletedTerm.reference("x")` remains a free stable reference and is rejected
by this definition constructor. Parameter and result types must be equal for
the parameter-reference body, and both must belong to the existing bounded
constructible type family.

The package-private compiler-free definition model additionally admits bodies
from the existing bounded internal term family, including:

```scala
def id(x: Int): Int = x
def inc(x: Int): Int = x + 1
def keep(x: String): String = x
```

The method name, one ordinary parameter name, structural parameter type,
structural result type, and existing definition-body subset are explicit. The
parameter declaration and its body references share the existing project-owned
binder identity. Therefore renaming `x` to `y` preserves structural equality
when corresponding references remain bound, while a free same-text identifier
remains distinct. Parameter/result types, the definition name, and non-bound
body structure remain significant.

The public result is a projection, not the internal definition value. It exposes
only the method kind, names, completed types, explicit body projection, and a
coherent source rendering. `BinderId`, `TermShape`, constructed/template
definitions, source maps, and compiler trees remain package-private.

This is not a general parameter-list model. Multiple clauses or parameters,
contextual/implicit/type parameters, defaults, varargs, by-name or erased
parameters, dependent methods, local definitions, binder-name holes, and
general owner/placement policy remain unsupported. Richer public bodies and a
source adapter are not implied. The existing located definition carrier cannot
truthfully describe both parameter and result-type spans, so it still rejects
this variant. Both exact definition backends also reject it explicitly; no
one-parameter `DefDef` lowering is claimed here.

Unsupported type inputs report the rejected constructor, selected syntax,
expected arity, or unsupported family where available. Located source adapters
use exact-occurrence precision only when the existing source map identifies a
single truthful occurrence; otherwise they use whole-source precision or no
location. Generated hole-transport identifiers are not part of public errors.

## Constructor expressions

Term construction and matching support one bounded form:

```scala
new java.lang.StringBuilder(16)
```

The class name must be fully qualified, plain, and non-generic, with exactly
one ordinary argument list. Arguments may use the already-supported term
subset. Imported/simple names, generic types, constructor holes, named or
multiple lists, and anonymous classes are rejected. This is constructor-only
resolution, not a general type-name resolver.
