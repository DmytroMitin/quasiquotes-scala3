package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

/** Test-only parser oracle for the complete AUXify-041 factory shape. */
class C011InstanceFactoryRawOracleTest extends munit.FunSuite:
  private val CanonicalSource =
    """def instance[A](emptyValue: => A, combineFunction: (A, A) => A): Monoid[A] = new Monoid[A] {
      |  override def empty: A = emptyValue
      |  override def combine(a: A, a1: A): A = combineFunction(a, a1)
      |}""".stripMargin

  private val ExpectedStructure =
    "DefDef(instance,129,List(List(TypeDef(A,259,TypeBounds(Empty,Empty,Empty))), List(ValDef(emptyValue,259,ByName(Ident(A)),Empty), ValDef(combineFunction,259,Function(List(Ident(A), Ident(A)),Ident(A)),Empty))),Applied(Ident(Monoid),List(Ident(A))),New(Template(DefDef(<init>,0,List(),Empty,Empty),List(Applied(Ident(Monoid),List(Ident(A)))),ValDef(_,8199,Empty,Empty),List(DefDef(empty,145,List(),Ident(A),Ident(emptyValue)), DefDef(combine,145,List(List(ValDef(a,259,Ident(A),Empty), ValDef(a1,259,Ident(A),Empty))),Ident(A),Apply(Ident(combineFunction),List(Ident(a), Ident(a1))))))))"

  test("records the exact raw parser topology of the complete factory") {
    withContext {
      val definition = parseOne(CanonicalSource)
      val observed = structure(definition)
      assertEquals(observed, ExpectedStructure)
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parsers.Parser(SourceFile.virtual("C011InstanceFactory.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.DefDef =>
        s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(structure))},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${value.mods.flags},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.ByNameTypeTree => s"ByName(${structure(value.result)})"
      case value: untpd.Function =>
        s"Function(${value.args.map(structure)},${structure(value.body)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Apply =>
        s"Apply(${structure(value.fun)},${value.args.map(structure)})"
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.New => s"New(${structure(value.tpt)})"
      case value: untpd.Template =>
        s"Template(${structure(value.constr)},${value.parentsOrDerived.map(structure)},${structure(value.self)},${value.body.map(structure)})"
      case value: untpd.Block =>
        s"Block(${value.stats.map(structure)},${structure(value.expr)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName
