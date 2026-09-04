# Bounded Scalameta Term to untyped bridge

`dottyInternal` exposes one exact-version public programmatic facade:

```scala
quasiquotes.terms.dotty.ScalametaTermUntypedBridge
  .lower(term)(using context)
```

The result is
`Either[ScalametaTermUntypedBridge.Failure, dotty.tools.dotc.ast.untpd.Tree]`.
Consumers must use the `dottyInternal` artifact built for the same full Scala
compiler version as their active `Context`.

## Owned composition

The facade mechanically composes the existing narrow layers:

```text
scala.meta.Term
  -> ScalametaTermProjection.project
  -> projected core TermShape
  -> CoreTermShapeUntypedLowerer.lower
  -> fresh source-free untpd.Tree
```

It does not render or reparse syntax, switch to a richer backend, recover with
another frontend, or perform name resolution, typing, symbol creation, owner
selection, or placement.

The admitted intersection is the direct non-binder family shared by both
layers: semantic Int/String/Boolean literals; direct identifiers; recursive
selections; one ordinary positional Apply list; recursive ordinary infix and
unary terms; tuples of arity 2 through 22; an `if` with an explicit `else`;
standard single-quoted `s` interpolation; one fully-qualified non-generic
constructor with exactly one ordinary positional argument list; transparent
P0 parentheses; and binder-free P1 blocks.

## Stable public failure boundary

`Failure.code` has three public classes:

- `MISSING_INPUT` — the supplied Term is absent;
- `NEUTRAL_PROJECTION_FAILED` — the bounded neutral projector rejected it;
- `EXACT_LOWERING_FAILED` — projection succeeded but the direct exact lowerer
  rejected the projected shape.

`Failure.detail` preserves bounded diagnostic context, but callers should
branch on `code` rather than exact prose.

Nested Apply lists, Type application, named or repeated arguments, contextual
argument clauses, simple/import-relative or type-applied constructors,
multiple constructor lists, anonymous templates, Type ascription, Lambda1,
P2/P3 binder blocks, and broader statement families fail closed. In
particular, the facade does not route P2/P3 through the richer internal backend
that accepts completed-Type sidecars.

## Source-free result contract

Every returned raw node has no source, no span, `NoSymbol`, and no
`TypedSplice`. The result is a fresh semantic reconstruction: it preserves no
Scalameta child identity and claims no source provenance. Ownership, insertion,
reownership, and lifecycle remain the consumer's responsibility.

Ordinary Typer accepts the tested source-free identifier, selection, Apply,
literal, tuple, conditional, interpolation, unary, constructor, and binder-free
block shapes when the enclosing context supplies the referenced names. A raw
span-free `untpd.InfixOp` is intentionally returned for infix syntax, but
Dotty's direct infix typing path reads spans; typing that node requires a
separate consumer-owned transformation or provenance policy.

The category-specific name is intended to scale with additional bounded Term
families without encoding arity or a rollout tranche. A future Type boundary
would use a separate Type-specific sibling facade. This API does not establish
a universal Scalameta-to-Dotty bridge and adds no `u*`/`n*` source syntax.
