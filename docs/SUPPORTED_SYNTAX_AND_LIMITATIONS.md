# Supported syntax and limitations

The implementation is a structural research subset, not a complete Scala 3
quasiquote system.

Currently exercised areas include:

- identifiers, selections, applications, typed terms, blocks, tuples, unary
  operations, and standard string interpolation in bounded structural forms;
- type identifiers, selections, applications, tuples, functions, wildcards,
  unions/intersections, annotations, refinements, and selected bounds/match
  forms;
- term and type holes with collision-safe rewriting and repeated-hole checks;
- compiler-free term/type/definition templates and completed values;
- one bounded public contextual-method construction contract;
- source spans, diagnostic anchors, and exact-version lowering adapters;
- recursively nested `List` and `Option` types plus binary `Either`, with
  structural argument order, repeated type holes, construction, quoted
  lowering/inspection, typed ascriptions, and scoped `Type[t]` evidence.

Important limitations:

- no claim of complete Scala grammar or compiler-tree coverage;
- no stable raw-tree public API;
- no general owner/symbol repair or arbitrary generated-definition placement;
- compiler-internal behavior is exact-version-sensitive;
- public definition construction is intentionally narrow;
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
