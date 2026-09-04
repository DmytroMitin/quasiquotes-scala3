# Bounded Scalameta Type to untyped bridge

`dottyInternal` exposes a Type-category exact-version programmatic facade:

```scala
quasiquotes.types.dotty.ScalametaTypeUntypedBridge.lower(sourceType)
```

The result is
`Either[ScalametaTypeUntypedBridge.Failure, dotty.tools.dotc.ast.untpd.Tree]`.
Lowering does not require a Dotty `Context`; consumers must nevertheless use
the `dottyInternal` artifact built for their exact Scala compiler version.

## Owned composition

The facade mechanically composes the existing narrow layers:

```text
scala.meta.Type
  -> ScalametaTypeNormalFormProjection.project
  -> ProjectedTypeNormalForm.normalForm
  -> CompletedTypeUntypedLowerer.lower
  -> fresh source-free untpd.Tree
```

It does not inspect Type syntax itself, render or reparse, resolve names, use a
resolved-Type environment, call Quotes/reflection, type the result, create
symbols, choose an owner, place the tree, or fall back to another backend.

The admitted intersection is recursive Int/String/Boolean; fixed-arity
`List`, `Option`, and `Either` applications; tuple syntax of arity two or
three; and function syntax of arity one or two. These families may nest.

## Stable public failure boundary

`Failure.code` has three public classes:

- `MISSING_INPUT` — the supplied Type is absent;
- `NEUTRAL_PROJECTION_FAILED` — the bounded neutral projector rejected it;
- `EXACT_LOWERING_FAILED` — projection succeeded but exact lowering rejected
  the resulting normal form.

`Failure.detail` preserves the underlying bounded diagnostic. Callers should
branch on `code`, not exact prose.

`AnyVal` demonstrates the layer distinction: the neutral normal form admits
it, while the exact lowerer rejects it with `EXACT_LOWERING_FAILED`. Other
simple names, selected/path Types and selected constructors, unsupported
generic constructors or arities, tuple arity outside two/three, function arity
outside one/two, and refinements, unions, intersections, wildcards, singleton,
dependent, and match-Type forms fail during neutral projection.

Tuple and function syntax retain their semantic categories. Explicit generic
applications such as `Tuple2[Int, String]`, `Function1[Int, String]`, and
`Function2[Int, String, Boolean]` are not recovered from constructor names and
fail during neutral projection.

## Source-free result contract

Every admitted result is a fresh raw Type tree with no source or span and no
`TypedSplice`. It preserves no Scalameta child identity and makes no source
reconstruction, generated-origin, symbol, typing, ownership, or placement
promise.

The category-specific name remains meaningful as recursive Type support grows
through zero, one, two, three, or N children. Current arity bounds stay in the
admission grammar rather than the public name. Term and Type remain separate
sibling facades because their source grammars and exact-lowering intersections
are different; this API does not establish a generic Scalameta-to-Dotty bridge
or add `u*`, `n*`, or Type-quasiquote syntax.
