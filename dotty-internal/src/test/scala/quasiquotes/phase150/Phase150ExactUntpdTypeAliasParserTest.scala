package quasiquotes.phase150

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

class Phase150ExactUntpdTypeAliasParserTest extends munit.FunSuite:
  private val Canonical =
    "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }"

  test("records the exact parser topology before any raw constructor is selected") {
    withContext {
      val parsed = parseOne(Canonical)
      assertEquals(
        structure(parsed),
        "TypeDef(Aux,0,TypeLambda(List(TypeDef(N,259,TypeBounds(Empty,Ident(Nat),Empty)), TypeDef(M,259,TypeBounds(Empty,Ident(Nat),Empty)), TypeDef(Out0,259,TypeBounds(Empty,Ident(Nat),Empty))),Refined(Applied(Ident(Add),List(Ident(N), Ident(M))),List(TypeDef(Out,0,Ident(Out0))))))"
      )
      assertEquals(
        spans(parsed),
        Vector(
          ("TypeDef", 0, 5, 73),
          ("LambdaTypeTree", 9, 9, 73),
          ("TypeDef", 9, 9, 17),
          ("TypeBoundsTree", 11, 11, 17),
          ("Ident", 14, 14, 17),
          ("TypeDef", 19, 19, 27),
          ("TypeBoundsTree", 21, 21, 27),
          ("Ident", 24, 24, 27),
          ("TypeDef", 29, 29, 40),
          ("TypeBoundsTree", 34, 34, 40),
          ("Ident", 37, 37, 40),
          ("RefinedTypeTree", 44, 44, 73),
          ("AppliedTypeTree", 44, 44, 53),
          ("Ident", 44, 44, 47),
          ("Ident", 48, 48, 49),
          ("Ident", 51, 51, 52),
          ("TypeDef", 56, 61, 71),
          ("Ident", 67, 67, 71)
        )
      )
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parseOne(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parser(SourceFile.virtual("Phase150AuxTypeAlias.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (value: untpd.TypeDef) :: Nil => value
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.LambdaTypeTree =>
        s"TypeLambda(${value.tparams.map(structure)},${structure(value.body)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.RefinedTypeTree =>
        s"Refined(${structure(value.tpt)},${value.refinements.map(structure)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def spans(tree: untpd.Tree)(using Context): Vector[(String, Int, Int, Int)] =
    if tree.isEmpty then Vector.empty
    else
      Vector((tree.getClass.getSimpleName, tree.span.start, tree.span.point, tree.span.end)) ++
        directChildren(tree).flatMap(spans)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.LambdaTypeTree => value.tparams.toVector :+ value.body
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty
