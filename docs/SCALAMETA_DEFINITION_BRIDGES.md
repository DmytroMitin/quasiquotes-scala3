# Bounded Scalameta Definition bridges

The unpublished `dottyInternal` source candidate exposes two public,
exact-compiler-version operations for bounded `scala.meta.Defn` input. They are
programmatic compiler-plugin seams, not new quasiquote syntax and not a general
raw-tree toolkit. A consumer must use the same full Scala compiler version and
provide an active Dotty `Context`.

## Source-free bridge

```scala
import dotty.tools.dotc.core.Contexts.Context
import quasiquotes.definitions.dotty.ScalametaDefinitionUntypedBridge
import scala.meta.Defn

def lower(definition: Defn)(using Context) =
  ScalametaDefinitionUntypedBridge.lower(definition)
```

The bridge mechanically projects through the common neutral Definition
dispatcher, lowers the resulting project-owned `DefinitionShape`, and verifies
that the result is an `untpd.MemberDef`. Every returned descendant is fresh,
source/span-free, symbol-free, and free of `TypedSplice`.

| Admitted family | Exact result |
| --- | --- |
| explicitly typed immutable `val` | `untpd.ValDef` |
| true parameterless explicitly typed `def` | `untpd.DefDef` |
| one ordinary explicitly typed parameter in one clause | `untpd.DefDef` |
| exactly two ordinary explicitly typed parameters in one clause | `untpd.DefDef` |
| simple non-generic unbounded Type alias | `untpd.TypeDef` |

The stable failure codes are `MISSING_INPUT`,
`NEUTRAL_PROJECTION_FAILED`, and `EXACT_LOWERING_FAILED`. A neutral projection
success can still fail exact lowering when its Type or Term components are
outside the exact lowerer's intersection.

## Generated-origin bridge

```scala
import dotty.tools.dotc.core.Contexts.Context
import quasiquotes.definitions.dotty.ScalametaDefinitionGeneratedOriginBridge
import scala.meta.Defn

def lowerForInsertion(definition: Defn, virtualSource: String)(using Context) =
  ScalametaDefinitionGeneratedOriginBridge.lower(definition, virtualSource)
```

This operation admits the first four concrete val/def families in the table.
It completes the projected semantics, applies the existing generated-origin
adapter, and returns `Lowered` with:

- `tree`: an `untpd.MemberDef` whose descendants carry the generated source;
- `generatedSource`: deterministic admitted Scala source;
- `sourceFile`: the effective virtual `SourceFile`;
- `virtualSourceName`: the source path derived from that `SourceFile`.

The additional stable failure codes are
`GENERATED_ORIGIN_FAMILY_UNSUPPORTED`, `DEFINITION_COMPLETION_FAILED`,
`INVALID_VIRTUAL_SOURCE`, and `GENERATED_ORIGIN_FAILED`. A simple Type alias is
always rejected with `GENERATED_ORIGIN_FAMILY_UNSUPPORTED`; the generic route
does not borrow authority from the richer specialized refined-alias pipeline.

## Consumer responsibilities

The bridges own bounded admission, projection-to-lowering composition,
categorized diagnostics, and the stated source-free or generated-origin
result. They do not own target admission, tree insertion, batch rollback,
ordinary typing, symbol creation, owner assignment, or reownership.

A plugin that inserts several generated definitions should therefore lower the
complete batch first, stop on the first failure, and mutate the target only
after every member succeeds. The plugin remains responsible for whether a
class, trait, companion, or other target is admissible and for running the
ordinary compiler lifecycle after insertion.

Unsupported shapes fail closed. These include `var`, untyped definitions,
empty-parens or multiple/contextual clauses, default arguments, generic or
bounded methods, broader bodies, generic/bounded aliases, classes, traits,
objects, and arbitrary statements. The exact admitted Type and Term fragments
remain those of the underlying project-owned models.

The current source candidate is validated on exact Scala 3.3.8, 3.8.4, and
3.9.0 lanes. The module remains remotely unpublished; local coordinate use is
an explicit source-candidate workflow, not a Maven Central compatibility
promise.
