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

Accepted production support is the recursive family formed from Scalameta
`Lit.Int`, `Lit.String`, and `Lit.Boolean` semantic values; ordinary binary
`Term.ApplyInfix`; unary `+`, `-`, `!`, and `~`; tuples of arity 2 through 22;
explicit three-branch `if`; conservative direct source identifiers and
selections; and one ordinary positional `Term.Apply` argument list. It also
admits one fully-qualified non-generic `Term.New` with exactly one ordinary
positional argument list, one explicitly typed ordinary Lambda1, transparent
one-Term P0 braces, binder-free P1 blocks, one bounded explicitly typed eager
immutable local-val P2 block with the existing whole-tree binder/shadowing
policy, and one bounded source-owned local identity-method P3 block. The P3 family requires one
modifier-free, non-generic, one-parameter method with explicit structurally
compatible Int/String/Boolean Types, a direct parameter body, a direct method
result, distinct deterministic binders, and no recursion. Apply and the admitted
constructor list permit zero, one, or multiple arguments. Nested Apply lists,
Type application, contextual clauses, simple/import-relative or type-applied
constructors, multiple constructor lists, named/star arguments, anonymous
templates, ascription, interpolation, and broader lambdas/binders/statements
return stable `NeutralProjectionError` categories;
they never become `TermShape.Unsupported`.

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
projection. The production term projector uses the bounded accepted family
described above with explicit name, clause, argument, binder, failure, and span
contracts. The prototype's one-list `new` remains test-only feasibility
evidence only for its broader unguarded shape; the accepted production subset
is the fully-qualified plain-name, non-generic, exactly-one-positional-list
family above. Lambda1 and P2 reuse existing Core binder identity; P2 declared
Types reuse the accepted neutral Type projection.

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
the exact bridges. For the production Term route, the accepted non-binder
literal/infix/unary/tuple/conditional/Identifier/Select/one-list Apply family,
plus transparent P0 and binder-free P1 blocks, is consumed by package-private
`CoreTermShapeUntypedLowerer`, which directly constructs corresponding
source-free raw nodes. Canonical literal text, the fixed ordinary operator and
unary sets, and direct ASCII non-keyword names are validated before raw names
or nodes are created. A direct nested Apply in function position is rejected
as multiple lists; Apply remains recursively valid in ordinary argument and
qualifier positions. Lambda1, P2, and P3 are outside this direct edge; P2 and
P3 also remain rejected by the richer exact backend.

The separate definition route reuses the existing validated-IR and
generated-origin adapters to produce a positioned `untpd.DefDef`. Reverse
definition projection structurally reconstructs only the admitted Scalameta
definition shape and deliberately returns `Position.None`.

Reverse projection cannot truthfully reconstruct source tokens, comments,
formatting, exact offsets, or compiler-normalized distinctions. Unsupported raw
forms fail explicitly. Exact trees never appear in the neutral module's API.
The Term route likewise does not carry Phase-140 Scalameta offsets through the
core value, fabricate source, or publish a `scala.meta.Term -> untpd.Tree` API.
It constructs new D syntax from project-owned semantics; it does not absorb the
separate U experiment for identity-preserving structural rewrites over existing
raw trees.

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
