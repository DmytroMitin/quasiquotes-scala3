# Execution environments and AST representations

Quasiquotes in this repository operate in several different execution
environments. Choose the environment first: a live quoted-reflection tree, a
neutral runtime value, and a compiler parser result have different ownership,
classpath, and execution rules.

| Example | Environment | Main value | Active `Quotes` | Evaluates generated Scala |
| --- | --- | --- | --- | --- |
| D1 | compile-time inline macro | `Expr[T]` / `quotes.reflect.Term` | supplied by macro expansion | the generated program executes normally |
| D2 | runtime `staging.withQuotes` | `Expr[T]` / `quotes.reflect.Term` | supplied at runtime | no |
| D3 | runtime `staging.run` | `Expr[T]` / `quotes.reflect.Term` | supplied at runtime | yes |
| D4 | ordinary compiler-free runtime | `TermShape` | no | no |
| D5 | compiler-backed runtime parsing | `ParsedExpression` plus raw tree and `TermShape` | no | no |
| D6 | compiler-free Scalameta authoring/matching | `scala.meta.Term`, `Type`, and `Defn` | no | no |

The examples below are compiled in the unpublished `public-core-examples` or
`public-api-examples` projects. `scala3-staging` is a test-only dependency of
the latter; it is not a dependency of the published `core` or `frontend`
artifacts.

## D1: compile-time `qr` construction and `qq` matching

An inline macro receives caller-owned `Expr` values. Its implementation works
in the compiler-provided `Quotes`, constructs a reflected addition with `qr`,
matches that tree with `qq`, and returns the constructed expression.

```scala
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.qr

object CompileTimeExample:
  inline def add(left: Int, right: Int): Int =
    ${ addImpl('left, 'right) }

  def addImpl(left: Expr[Int], right: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.matching.QuasiPattern.qq

    val tree = qr"${left.asTerm} + ${right.asTerm}"
    tree match
      case qq"$capturedLeft + $capturedRight" =>
        tree.asExprOf[Int]
      case _ =>
        quotes.reflect.report.errorAndAbort("unexpected qr result")
```

`CompileTimeExample.add(1, 2)` is tested to produce `3`. The captures remain in
the caller's path-dependent `Quotes` universe. Matching projects the original
target subtrees after the matcher's documented transparent-wrapper removal; it
does not serialize them through a neutral AST.

## D2: runtime inspection with `staging.withQuotes`

Runtime staging can provide a temporary `Quotes` context without compiling and
running the resulting expression. This is useful for construction, inspection,
matching, and rendering.

```scala
import scala.quoted.{Expr, Quotes, staging}

def inspect(left: Int, right: Int): (String, String) =
  given staging.Compiler =
    staging.Compiler.make(getClass.getClassLoader)

  staging.withQuotes:
    inspectImpl(left, right)

private def inspectImpl(left: Int, right: Int)(using q: Quotes): (String, String) =
  import q.reflect.*
  import quasiquotes.construct.Quasiquotes.qr
  import quasiquotes.matching.QuasiPattern.qq

  val tree = qr"${Expr(left).asTerm} + ${Expr(right).asTerm}"
  tree match
    case qq"$capturedLeft + $capturedRight" =>
      (capturedLeft.show, capturedRight.show)
    case _ =>
      throw new AssertionError("unexpected mismatch")
```

The tested result for `(1, 2)` is `("1", "2")`. `withQuotes` does not evaluate
the addition. Runtime-created terms commonly have `NoSpan`; structural `qq`
matching therefore treats source provenance as optional. Standard-`s`
interpolation still uses source-sensitive evidence when truthful source text is
available.

## D3: runtime compilation and evaluation with `staging.run`

`staging.run` performs the additional compilation and evaluation step:

```scala
import scala.quoted.{Expr, staging}

def runAdd(left: Int, right: Int): Int =
  given staging.Compiler =
    staging.Compiler.make(getClass.getClassLoader)

  staging.run:
    CompileTimeExample.addImpl(Expr(left), Expr(right))
```

The compiled regression proves `runAdd(1, 2) == 3`. Runtime staging requires a
compiler-compatible `frontend` and `scala3-staging` on the runtime classpath;
it is not a compiler-free mode.

## D4: ordinary compiler-free `TermShape` data

`TermShape` is neutral runtime data. It can be created and matched with
ordinary Scala code:

<!-- snippet:runtime-term-shape:start -->
```scala
import quasiquotes.parser.TermShape

object RuntimeTermShapeExample:
  val tree: TermShape = TermShape.Infix(
    TermShape.Literal("1"),
    "+",
    TermShape.Literal("2")
  )

  val literalOperands: Option[(String, String)] =
    tree match
      case TermShape.Infix(
            TermShape.Literal(left),
            "+",
            TermShape.Literal(right)
          ) => Some((left, right))
      case _ => None
```
<!-- snippet:runtime-term-shape:end -->

This value is not a live typed compiler AST. It has no owner, symbol, or
`Quotes` universe, and the library does not automatically execute it as Scala
code.

## D5: compiler-backed parsing without `Quotes`

`TinyTermParser` is callable at ordinary runtime without an active `Quotes`:

<!-- snippet:runtime-parser:start -->
```scala
import quasiquotes.parser.{ParsedExpression, TinyTermParser}

object RuntimeParserExample:
  val parsed: Either[Throwable, ParsedExpression] =
    TinyTermParser.parse("1 + 2")

  val summary: Either[Throwable, (String, String, String)] =
    parsed.map { result =>
      (
        result.source,
        result.shape.render,
        result.rawTree.getClass.getName
      )
    }
```
<!-- snippet:runtime-parser:end -->

`ParsedExpression` retains the original source, the compiler-internal
`rawTree`, a public neutral `shape`, and a raw-structure rendering. This mode is
not compiler-free: the parser and raw tree are Dotty compiler internals owned by
the compiler-coupled `frontend` module. Parsing does not typecheck or execute
the expression.

## D6: compiler-free Scalameta source AST

The unpublished `neutralScalameta` module uses Scalameta 4.17.3 directly for
source construction and extractor matching. It requires neither an active
`Quotes` nor `scala3-staging`, and its dependency boundary excludes the Scala
compiler implementation and SemanticDB.

Scalameta trees are source syntax, not typed reflection or exact Dotty trees.
`ScalametaTermProjection` admits exactly semantic integer literals and ordinary
one-RHS binary infix nodes into the existing core `TermShape`, preserving a
truthful root span when present and failing closed otherwise. The separate
`ScalametaContextualMethodProjection` admits one contextual method into the
existing validated definition IR. Exact lowering and reverse raw-tree
projection remain in `dottyInternal`. See the
[neutral Scalameta experiment](NEUTRAL_SCALAMETA_EXPERIMENT.md).

## Current representation inventory

### Terms

| Representation | Visibility and role |
| --- | --- |
| `TermShape` | Public neutral structural/parser shape in `core`; compiler-free ordinary data. |
| `TermPattern` / `MatchResult` | Public neutral structural matching data in `core`. |
| `TermTemplate` / `ConstructedTerm` | Package-internal validated construction IR in `core`; deliberately not public. |
| `CompletedTerm` | Public bounded definition-body payload; not the general term AST. |
| `scala.meta.Term` | Experimental unpublished source-syntax term in `neutralScalameta`; direct construction/matching, with production projection to `TermShape` limited to semantic integers and ordinary binary infix nodes. |
| `Expr[T]` / `quotes.reflect.Term` | Caller-`Quotes` staged and reflected values used by `qr`/`qq`; compiler-coupled and universe-dependent. |
| `dotty.tools.dotc.ast.untpd.Tree` | Exact compiler-internal value used by parsing and the unpublished exact backend; not a published AST contract. |

### Types

| Representation | Visibility and role |
| --- | --- |
| `TypeShape` | Public parser/structural shape. |
| `TypeNormalForm` | Public neutral semantic structural form. |
| `TypePattern`, `TypeTemplate`, `ConstructedType` | Public neutral matching and construction machinery. |
| `CompletedType` | Public bounded definition payload; not the universal type AST. |
| `scala.meta.Type` | Experimental unpublished source-syntax type in `neutralScalameta`; not semantically resolved. |
| `TypeRepr`, `TypeTree`, `Type[T]` | Caller-`Quotes` reflected/staged values used by the reflected type surfaces. |
| compiler-internal type trees | Exact unpublished backend/parser values with compiler-version coupling. |

### Definitions

The definition stack is intentionally less public. `DefinitionShape`,
`DefinitionTemplate`, `ConstructedDefinition`, and their assembly/evidence
types are package-internal. Public compiler-free construction is limited to
`DefinitionConstruction` and the bounded `DefinitionResultView`,
`SingleParameterMethodResultView`, and `TwoParameterMethodResultView`
projections. Public reflected `dqr`/`dqq` are bounded `DefDef` surfaces in the
caller's `Quotes`. Experimental `scala.meta.Defn` values supply direct neutral
source construction/matching in the unpublished module, and only the admitted
contextual-method shape projects into `DefinitionResultView`. The exact
raw/generated-origin definition backend remains unpublished.

Not every internal representation warrants another interpolation syntax. Local
imports can rename Scalameta `q`/`t` to provisional `nqr`/`nqq`,
`ntqr`/`ntqq`, and `ndqr`/`ndqq` spellings, but the project exports no such
members: forwarding the macro interpolators is rejected upstream, and copying
their implementation is outside this experiment.

Those `ndqr`/`ndqq` names are only consumer aliases for upstream untyped
Scalameta `q` definition syntax. The typed Scalameta opt-in exports `qr`/`qq`
and `tqr`/`tqq` only; it has no typed `dqr`/`dqq`. A future typed Definition
frontend must reuse the project-owned Definition model and a backend ownership
plan rather than treating a Scalameta `Defn` as an insertion-ready reflected
definition.

For reflected Type composition, `TypeRepr` is the semantic transport in one
active `Quotes` universe. `TypeTree.tpe`, `TypeRepr.of[T]`, and `tqr` all
produce that same kind of value. A future `qr` constructor-Type hole is
therefore designed to accept `TypeRepr` directly, while the compiler-free core
uses only a generic internal payload slot and retains its compiler boundary.

## One operation at several representation levels

The same source-level addition can be assembled at several boundaries, but
those boundaries are not interchangeable APIs. The first three forms below
are supported public Scala/Quasiquotes use. The last three are exact-compiler
demonstrations used by this repository's tests and unpublished backend.

### Supported public APIs

At the standard quoted level, an inline macro can preserve its typed operands
directly:

```scala
def addExpr(left: Expr[Int], right: Expr[Int])(using Quotes): Expr[Int] =
  '{ $left + $right }
```

At the public reflection level, the operands are `quotes.reflect.Term`.
`Select.overloaded` performs the member and overload selection needed by `+`:

```scala
def addTerm(using q: Quotes)(
    left: q.reflect.Term,
    right: q.reflect.Term
): q.reflect.Term =
  import q.reflect.*
  Select.overloaded(left, "+", Nil, List(right))
```

Quasiquotes `qr` works at this same reflected-Term level and preserves the
caller-owned operands:

```scala
def addTerm(using q: Quotes)(
    left: q.reflect.Term,
    right: q.reflect.Term
): q.reflect.Term =
  import quasiquotes.construct.Quasiquotes.qr
  qr"$left + $right"
```

External macro tests compile and execute the standard quotation, public
reflection, and `qr` forms on every supported compiler line; each returns `3`
for operands `1` and `2`.

### Exact-version internal demonstrations

In the current compiler implementation, `QuotesImpl` implements reflected
trees with `tpd.Tree`. An exact-version test can therefore obtain the
implementation value with
`left.asTerm.asInstanceOf[tpd.Tree]`. `ExprImpl.tree` is also a `tpd.Tree`, but
constructing `ExprImpl` manually would additionally require a correct active
splice scope. The positive return path used here is instead:

```text
tpd.Tree -> the active q.reflect.Term implementation -> asExprOf[Int]
```

For typed construction, Scala 3.3.8, 3.8.4, and 3.9.0-RC1 expose the same
exact internal call:

```scala
tpd.applyOverloaded(
  left,
  "+".toTermName,
  List(right),
  Nil,
  Types.WildcardType
)
```

The parameter order is receiver, method name, value arguments, type
arguments, and expected type. The result is a `tpd.Tree`. Public
`Select.overloaded` delegates to this operation on all three lines. A bare
`tpd.Apply(tpd.Select(left, "+"), List(right))` only puts typed-looking nodes
together; it does not perform the member lookup, alternative selection, and
application adaptation performed by `applyOverloaded`, and can yield an
invalid typed result such as a selected `+` that is then applied incorrectly.

The separate mixed typed/untyped probe creates new untyped syntax while
retaining typed operands as leaves:

```scala
val shell = untpd.Apply(
  untpd.Select(untpd.TypedSplice(left), "+".toTermName),
  List(untpd.TypedSplice(right))
)
val typed = new Typer().typedExpr(shell)
```

This is not a conversion or inverse map from `tpd.Tree` to `untpd.Tree`.
`TypedSplice` marks an already typed subtree embedded in a newly created
untyped shell, and the exact compiler typer produces a new typed result.

At a pure raw-syntax boundary, valid raw leaves can be placed into the
expected parser-like shell without claiming that it is already typed:

```scala
def addUntpdTree(
    left: untpd.Tree,
    right: untpd.Tree
)(using SourceFile): untpd.Tree =
  untpd.Apply(
    untpd.Select(left, "+".toTermName),
    List(right)
  )
```

Macro-Paradise annotation expansion operates on pre-typer `untpd` syntax.
Its plugin and handler lifecycle owns admission, placement, insertion,
rollback, and ordinary typing around handler output. This does not make
Quasiquotes a Macro-Paradise product dependency; the current peer integration
is a narrow data flow through an unpublished exact-version bridge.

The typed path is a ladder inside one active compiler context:

```text
standard quoted API: Expr[T]
  |
  v
quotes.reflect.Term ---------------- Quasiquotes qr/qq work here
  |
  | exact-version implementation detail
  v
tpd.Tree
  |
  | typed leaves embedded; no inverse conversion
  v
untpd.TypedSplice(tpd.Tree) inside a new untpd.Tree
  |
  | exact compiler Typer
  v
tpd.Tree
  |
  v
quotes.reflect.Term / Expr[T]
```

The neutral/source route is a separate axis, not an intermediate rung in that
ladder:

```text
scala.meta AST
  |
  v
neutralScalameta validated projection
  |
  v
project-owned compiler-free core IR
  |
  v
dottyInternal exact lowering for an admitted peer operation
  |
  v
untpd.Tree
```

The current production endpoint of this second route is one contextual
method, not a general Term bridge. See the
[Dotty-internal exact backend](DOTTY_INTERNAL_BACKEND.md).

## Exact-version `Quotes` and Dotty-internal interoperability

Scala 3 `Quotes` is implemented over Dotty's typed trees, so a technical bridge
between public reflection and compiler internals is possible. A test-only
feasibility probe has exercised this exact-version path on the repository's
validated compiler lines:

```text
Expr / q.reflect.Term
  -> underlying tpd.Tree
  -> untpd.TypedSplice leaves
  -> newly constructed untpd shell
  -> new Typer().typedExpr
  -> tpd.Tree
  -> q.reflect.Term / Expr
```

This is experimental evidence, not a supported public bridge. Casting through
`QuotesImpl` and `tpd.Tree`, constructing `untpd.TypedSplice`, and invoking
`Typer` directly are compiler-internal operations coupled to the exact Scala
version and active compiler context. There is no generally meaningful inverse
from a typed `tpd.Tree` to its original untyped tree: typing changes and adds
information. `TypedSplice` is instead the compiler's appropriate mechanism for
embedding an already typed subtree as a leaf in a newly built untyped shell.

The production `dottyInternal` `ConstructedTermUntypedBackend` has a narrower
job: it lowers the compiler-free `ConstructedTerm` model. It does not expose a
generic bridge for arbitrary typed-tree leaves. If a concrete normal-macro
consumer eventually justifies this escape hatch, the public boundary should
be one narrow, exact-version operation with owned validation and context rules,
not a general raw-tree toolkit.

## Source provenance policy

Typed structural matching does not require a source file. If a reflected
position provides usable, truthful source text, the frontend may use it for
source-sensitive standard-`s` interpolation evidence. If a position has
`NoSpan`, or its source text is otherwise unavailable, that optional evidence
is absent and ordinary structural matching continues.

On the tested Scala 3.3.8, 3.8.4, and 3.9.0-RC1 lanes,
`Quotes.Position.sourceCode: Option[String]` can itself assert while reading a
valid generated `NoSpan`; the public position API exposes no availability
predicate. The frontend therefore guards only the two known `start/end of
NoSpan` assertions around optional provenance reads. Other compiler failures
are not converted into missing provenance.
