# Bounded Scalameta Term generated-origin bridge

The unpublished `dottyInternal` source candidate exposes a public,
exact-compiler-version operation for turning one admitted `scala.meta.Term`
into a positioned fresh raw tree:

```scala
import dotty.tools.dotc.core.Contexts.Context
import quasiquotes.terms.dotty.ScalametaTermGeneratedOriginBridge
import scala.meta.Term

def lowerForInsertion(term: Term, virtualSource: String)(using Context) =
  ScalametaTermGeneratedOriginBridge.lower(term, virtualSource)
```

The consumer must use the `dottyInternal` artifact for the same full Scala
compiler version as its active `Context`.

## Owned composition

The facade is a mechanical composition of existing bounded authorities:

```text
scala.meta.Term
  -> ScalametaTermProjection.project
  -> ConstructedTerm.fromShape
  -> ConstructedTermGeneratedOriginAdapter.lower
  -> positioned untpd.Tree + generated source + SourceFile
```

It does not duplicate Scalameta grammar, render and reparse as a fallback, or
route through `ScalametaTermUntypedBridge`. The source-free bridge remains the
narrow direct projector/lowerer intersection; this generated-origin operation
uses the existing completed-term backend deliberately.

## Verified intersection

The generated-origin bridge admits the neutral projector's ordinary direct
family: Int/String/Boolean literals, identifiers, recursive selections, one
ordinary positional Apply list, recursive infix and unary terms, tuples of
arity 2 through 22, explicit `if`/`else`, standard single-quoted `s`
interpolation, and one fully-qualified non-generic constructor with one
ordinary positional list. Transparent P0 parentheses and binder-free P1 blocks
are also admitted.

The completed path additionally admits bounded Type ascriptions whose Types
can be completed by the public path (`Int`, `String`, and `Boolean`), one
ordinary explicitly typed Lambda1, one explicitly typed eager immutable
local-val P2 block, and the source-owned local identity-method P3 block. P3's
final result is exactly the local method reference, not an invocation. The
generated source is deterministic and may be canonical rather than byte-equal
to the input; for example `(1: Int)` becomes `(1): Int`.

Projection still rejects multiple Apply lists, Type application,
named/repeated/contextual arguments, non-`s` interpolation, simple or generic
constructor names, multiple constructor lists, anonymous templates, and
broader binders or statements. A projectable ascription such as
`(value: Option[Int])` fails completion because this facade does not accept an
external completed-Type sidecar.

## Result and stable failures

Success returns `Lowered` with:

- `tree`: the fresh positioned `untpd.Tree`;
- `generatedSource`: the deterministic source used for positioning;
- `sourceFile`: the effective virtual `SourceFile`;
- `virtualSourceName`: `sourceFile.path`.

Every returned node uses that source, has a truthful in-bounds span,
`NoSymbol`, and no `TypedSplice`. The stable `Failure.code` values are:

- `MISSING_INPUT`;
- `NEUTRAL_PROJECTION_FAILED`;
- `TERM_COMPLETION_FAILED`;
- `INVALID_VIRTUAL_SOURCE`;
- `GENERATED_ORIGIN_FAILED`.

Callers should branch on `code`, not exact diagnostic prose. No currently
admitted public input is known to reach `GENERATED_ORIGIN_FAILED`; the code is
the defensive public classification for non-name failures from the private
origin authority.

## Consumer responsibilities

The facade owns projection, completed-term construction, generated-source
planning, positioning, and categorized failures. It does not own target
admission, insertion, batch rollback, ordinary typing, symbol creation, owner
assignment, or reownership. Those remain compiler-plugin responsibilities.

This is a programmatic exact-version seam. It adds no `u*` or `n*` source
syntax, no public raw-tree builder, no general Scalameta-to-Dotty promise, and
no remote-release claim. The current source candidate is tested on exact Scala
3.3.8, 3.8.4, and 3.9.0.
