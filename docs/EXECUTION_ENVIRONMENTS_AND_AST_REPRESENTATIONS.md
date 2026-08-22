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
The bounded `ScalametaContextualMethodProjection` can admit one contextual
method into the existing validated core IR. Exact lowering and reverse raw-tree
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
| `scala.meta.Term` | Experimental unpublished source-syntax term in `neutralScalameta`; direct Scalameta construction and matching. |
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
