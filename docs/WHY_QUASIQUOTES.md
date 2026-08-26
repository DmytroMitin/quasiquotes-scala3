# Why quasiquotes?

Scala 3 quotations are the right default when the generated source shape is
fixed and well typed. For example, an inline macro can usually return a fixed
anonymous subclass directly:

```scala
'{
  new Runnable:
    def run(): Unit = println("hello")
}
```

This project becomes useful when a macro already works with
`quotes.reflect.Term` or `TypeRepr`, wants structural source-like matching, or
would otherwise spell nested reflection nodes and existential `asType`
bridges. It is a bounded structural tool, not a claim that `qr` is always
better than `'{ ... }`.

## Current compile-checked comparisons

The complete example below is compiled and run from an external-package test
fixture on the supported compiler lines.

<!-- snippet:why-quasiquotes-current:start -->
```scala
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern.*
import quasiquotes.types.QuasiTypequotes.*

object WhyQuasiquotesCurrentExamples:
  inline def standardAdd(left: Int, right: Int): Int =
    ${ standardAddImpl('left, 'right) }

  inline def quasiquoteAdd(left: Int, right: Int): Int =
    ${ quasiquoteAddImpl('left, 'right) }

  inline def manualSplit(left: Int, right: Int): (Int, Int) =
    ${ manualSplitImpl('left, 'right) }

  inline def quasiquoteSplit(left: Int, right: Int): (Int, Int) =
    ${ quasiquoteSplitImpl('left, 'right) }

  inline def nestedTypeConstructionAgrees: Boolean =
    ${ nestedTypeConstructionAgreesImpl }

  inline def nestedTypePatternAgrees: Boolean =
    ${ nestedTypePatternAgreesImpl }

  private def standardAddImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using Quotes): Expr[Int] =
    '{ $left + $right }

  private def quasiquoteAddImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*
    qr"${left.asTerm} + ${right.asTerm}".asExprOf[Int]

  private def manualSplitImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    unwrap(using q)('{ $left + $right }.asTerm) match
      case Apply(Select(foundLeft, "+"), List(foundRight)) =>
        val unwrappedLeft = unwrap(using q)(foundLeft)
        val unwrappedRight = unwrap(using q)(foundRight)
        '{ (${unwrappedLeft.asExprOf[Int]}, ${unwrappedRight.asExprOf[Int]}) }
      case other =>
        report.errorAndAbort(
          s"unexpected reflected addition: ${other.show(using Printer.TreeStructure)}"
        )

  private def unwrap(using q: Quotes)(tree: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    tree match
      case Inlined(_, _, inner) => unwrap(inner)
      case Block(Nil, inner: Term) => unwrap(inner)
      case _ => tree

  private def quasiquoteSplitImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using Quotes): Expr[(Int, Int)] =
    import quotes.reflect.*
    '{ $left + $right }.asTerm match
      case qq"$foundLeft + $foundRight" =>
        '{ (${foundLeft.asExprOf[Int]}, ${foundRight.asExprOf[Int]}) }
      case other =>
        report.errorAndAbort(s"unexpected quasiquote mismatch: ${other.show}")

  private def nestedTypeConstructionAgreesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val left: TypeRepr = TypeRepr.of[Int]
    val right: TypeRepr = TypeRepr.of[String]
    val sourceLike: TypeRepr = tqr"Either[List[$left], Option[$right]]"
    val manual: TypeRepr =
      left.asType match
        case '[l] =>
          right.asType match
            case '[r] => TypeRepr.of[Either[List[l], Option[r]]]
    Expr(sourceLike =:= manual)

  private def nestedTypePatternAgreesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val target: TypeRepr = TypeRepr.of[Either[List[Int], Option[String]]]
    val sourceLike = target match
      case tqq"Either[List[$left], Option[$right]]" =>
        left =:= TypeRepr.of[Int] && right =:= TypeRepr.of[String]
      case _ => false
    val manual = target match
      case AppliedType(eitherType, List(
            AppliedType(listType, List(left)),
            AppliedType(optionType, List(right))
          )) =>
        eitherType.typeSymbol.fullName == "scala.util.Either" &&
          listType.typeSymbol.fullName == "scala.collection.immutable.List" &&
          optionType.typeSymbol.fullName == "scala.Option" &&
          left =:= TypeRepr.of[Int] && right =:= TypeRepr.of[String]
      case _ => false
    Expr(sourceLike && manual)
```
<!-- snippet:why-quasiquotes-current:end -->

The first pair is deliberately unexciting: for the fixed addition shape,
standard quotation is shorter. `qr` is useful when the surrounding workflow
already has reflected Terms or when its supported source-like structural
surface composes more clearly with the rest of the macro.

The matching pair shows the present `qq` value: it replaces explicit
`Apply(Select(...), List(...))` decomposition with source-like structure and
returns the original reflected subtrees. Standard quoted patterns can also be
a good choice for statically typed shapes; `qq` is most useful in an existing
reflection-tree workflow that wants this project's bounded structural rules.

The Type construction comparison is a stronger current advantage. With
`left` and `right` already represented as `TypeRepr`,

```scala
tqr"Either[List[$left], Option[$right]]"
```

avoids two nested `asType` matches and existential type binders. The matching
comparison similarly replaces nested `AppliedType` decomposition with a
source-like `tqq` pattern while preserving the exact captured `TypeRepr`
subtrees.

## Computed selected-member names

Fixed member source shape still belongs in a standard quotation:

```scala
'{ $receiver.ordinary($argument) }
```

When the decoded member name is computed during macro expansion, manual
reflection previously remained necessary:

```scala
val selected = SelectedMemberName.from(computed).toOption.get
Select.unique(receiver.asTerm, selected.decoded).appliedTo(argument.asTerm)
```

The construction quasiquote now supports that exact bounded gap:

```scala
import quasiquotes.construct.Quasiquotes.*
import quasiquotes.construct.SelectedMemberName

val selected = SelectedMemberName.from(computed).fold(
  failure => report.errorAndAbort(failure.message),
  identity
)
qr"${receiver.asTerm}.$selected(${argument.asTerm})"
```

The external-package fixture compiles and executes the fixed quotation,
manual `Select.unique`, ordinary dynamic call, symbolic `+`, decoded keyword,
and safe spaced-name cases. The value inserts one validated decoded name into
an explicit selection. It does not search lexical scope, resolve aliases or
symbols by string, choose overloads, admit dynamic infix syntax, or add name
capture to `qq`.

## Roadmap examples are not current syntax

Bare dynamic identifier holes, constructor-position Type holes, and generic class
or anonymous-subclass definition quasiquotes would make the advantage over
manual reflection more dramatic. They are deliberately documented as
[north-star quasiquote checkpoints](NORTH_STAR_QUASIQUOTE_EXAMPLES.md), not as
current support or selected final syntax. See the
[syntax matrix](SYNTAX_SUPPORT_MATRIX.md) for the actual admitted language.
