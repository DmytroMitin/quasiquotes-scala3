# Neutral Scalameta experiment

The `neutralScalameta` sbt project is an unpublished, compiler-free source-AST
experiment. It depends on `core` and `org.scalameta:scalameta_3:4.17.3`, uses
ordinary Scala 3 binary crossing, and is aggregated by the root build. It has no
Scala compiler implementation, `scala3-staging`, or SemanticDB dependency.

This module is not part of the published `0.2.0` coordinate set. Its package,
result types, error codes, and any local syntax aliases are unstable research
interfaces rather than a compatibility commitment.

## Authoring and matching

Source construction and matching use Scalameta trees and quasiquotes directly.
The supported compile-time pattern imports the standard singleton dialect:

```scala
import scala.meta.*
import scala.meta.dialects.Scala3

val method = q"def apply[A](using inst: Show[A]): Show[A] = inst"
```

A production term-only hello world now uses the project's bounded projector
without an active `Quotes`:

```scala
import scala.meta.*
import scala.meta.dialects.Scala3
import quasiquotes.neutral.ScalametaTermProjection

val source: Term = q"obj.f(1 + 2, 3)"
val shape = ScalametaTermProjection.project(source).map(_.shape)
```

The result is:

```scala
Right(
  quasiquotes.parser.TermShape.Apply(
    quasiquotes.parser.TermShape.Select(
      quasiquotes.parser.TermShape.Identifier("obj", false),
      "f"
    ),
    List(
      quasiquotes.parser.TermShape.Infix(
        quasiquotes.parser.TermShape.Literal("1"),
        "+",
        quasiquotes.parser.TermShape.Literal("2")
      ),
      quasiquotes.parser.TermShape.Literal("3")
    )
  )
)
```

Production support is exactly the recursive family formed from Scalameta
`Lit.Int` semantic values, ordinary binary `Term.ApplyInfix` nodes with no Type
arguments and exactly one non-contextual RHS argument, conservative direct
source identifiers, direct selections, and one ordinary positional
`Term.Apply` argument list. Apply permits zero, one, or multiple arguments.
Nested Apply lists, Type application, contextual clauses, named/star
arguments, `new`, unary, tuple, `if`, block, ascription, interpolation, binder,
lambda, and placeholder shapes return a stable `NeutralProjectionError`.
They never become `TermShape.Unsupported`.

The name policy is syntactic only: direct and selected names use non-keyword
ASCII spellings matching `[A-Za-z_][A-Za-z0-9_]*`, excluding `_`. No compiler,
symbol, package/classpath, accessibility, uniqueness, overload, or future
typechecking claim follows from admission. A positioned root retains its exact
Scalameta offsets; a constructed `Position.None` root remains valid with no
span. Recursive children are semantic copies. The result preserves neither
Scalameta child identity nor raw Dotty subtree identity and carries no raw
sidecar or opaque exact island.

Pure upstream Scalameta construction and matching remains useful context and
also needs no project façade or active `Quotes`:

```scala
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.meta.quasiquotes.{q as nqr}
import scala.meta.quasiquotes.{q as nqq}

val left: Term = Lit.Int(20)
val right: Term = Lit.Int(22)
val addition: Term = nqr"$left + $right"
val sum = addition match
  case nqq"${Lit.Int(a)} + ${Lit.Int(b)}" => a + b
```

Here `nqr` and `nqq` are aliases chosen by the consumer for Scalameta's
upstream `q` macro. They are not members supplied by this library.

Scalameta's `q` and `t` macros already own grammar parsing, extractors, splice
ranks, repeated splices, dialect handling, and printing. The project does not
copy or fork those internals.

Consumer-local aliases can demonstrate the provisional vocabulary:

```scala
import scala.meta.quasiquotes.{q as nqr}
import scala.meta.quasiquotes.{q as nqq}
import scala.meta.quasiquotes.{t as ntqr}
import scala.meta.quasiquotes.{t as ntqq}
import scala.meta.quasiquotes.{q as ndqr}
import scala.meta.quasiquotes.{q as ndqq}
```

These aliases reuse the upstream macros and cover term, type, and definition
construction/matching, including repeated argument/member splices. They are
local import names, not exported library members. A reusable project `export`
of `q` or `t` is rejected by Scalameta's macro guard because those methods must
be invoked as string interpolators. The module therefore does not claim a thin
`n*` façade; direct Scalameta authoring remains the admitted mechanism.

## Bounded validated projection

`ScalametaContextualMethodProjection.project` structurally admits exactly one
generic method with one unmodified type parameter, one `using` parameter,
named/applied types, an explicit result type, and an identifier body. The
realistic supported value is:

```scala
def apply[A](using inst: Show[A]): Show[A] = inst
```

Unlike the pure upstream term hello world above, this example exercises the
project's current neutral module:

```scala
import scala.meta.*
import scala.meta.dialects.Scala3
import quasiquotes.neutral.ScalametaContextualMethodProjection

val source: Defn.Def =
  q"def apply[A](using inst: Show[A]): Show[A] = inst"
    .asInstanceOf[Defn.Def]

val projected = ScalametaContextualMethodProjection.project(source)
val name = projected.map(_.result.name)                 // Right("apply")
val parameter = projected.map(_.result.contextualParameterName) // Right("inst")
val resultType = projected.map(_.result.resultType.source)       // Right("Show[A]")
```

This exact example is compile-checked and remains a separate definition
projection. The production term projector now promotes the Phase-131
identifier, select, and one-list Apply evidence with stricter name, clause,
argument, failure, and span contracts. The prototype's one-list `new` remains
test-only feasibility evidence rather than production support. Binder identity
and completed Type sidecars still need their own bounded contracts.

The projector converts fields directly to `CompletedType`, `CompletedTerm`,
and `DefinitionConstruction.contextualMethod`. Unsupported shapes return
stable `NeutralProjectionError` categories. It performs no source rendering,
reparsing, name resolution, typechecking, symbol lookup, owner inference, or
compiler-context synthesis.

If the Scalameta input has an actual position, the result preserves its exact
start/end offsets as `NeutralSourceSpan`. Explicitly constructed trees with
`Position.None` remain valid and return no source span.

## Exact backend boundary

The unpublished `dottyInternal` module depends on `neutralScalameta` and owns
the exact bridges. For the production Term route, only the projected
integer/infix `TermShape` subset is consumed by package-private
`CoreTermShapeUntypedLowerer`, which directly constructs the corresponding
source-free `untpd.Number` and `untpd.InfixOp` nodes. Canonical signed decimal
text and a fixed ordinary operator set are validated before names or raw nodes
are created. Newly projected Identifier, Select, and Apply shapes reach that
exact boundary and fail with its existing unsupported-shape result; no exact
lowerer was added for them.

The separate definition route reuses the existing validated-IR and
generated-origin adapters to produce a positioned `untpd.DefDef`. Reverse
definition projection structurally reconstructs only the admitted Scalameta
definition shape and deliberately returns `Position.None`.

Reverse projection cannot truthfully reconstruct source tokens, comments,
formatting, exact offsets, or compiler-normalized distinctions. Unsupported raw
forms fail explicitly. Exact trees never appear in the neutral module's API.
The Term route likewise does not carry Phase-140 Scalameta offsets through the
core value, fabricate source, or publish a `scala.meta.Term -> untpd.Tree` API.

The exact module's complete ownership and exclusions are documented in the
[Dotty-internal exact backend](DOTTY_INTERNAL_BACKEND.md).

## Dialect boundary

Compile-time quasiquotes use the imported standard `Scala3` singleton. Explicit
parser tests use a `Scala38` value for `enum`/`derives`, `extension`, `using`,
`given`, and opaque types, and a separate `Scala3Future` value for a tracked
parameter. Parsing these forms is a syntax-compatibility check, not a claim
that the bounded projector semantically understands them.

The experiment does not implement annotation lifecycle, companion creation or
merge, placement, ordering, rollback, or downstream annotation semantics. It
does not add a public exact-untyped family or a separate exact typed-tree
family.
