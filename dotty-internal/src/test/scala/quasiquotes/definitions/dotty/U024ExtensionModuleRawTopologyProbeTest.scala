package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

/** Disposable failing oracle used to capture the raw parser topology for U024. */
class U024ExtensionModuleRawTopologyProbeTest extends munit.FunSuite:
  private val fixtures = Vector(
    """object syntax:
      |  extension [A](receiver: A)
      |    def combine(argument: A)(using evidence: Semigroup[A]): A =
      |      evidence.combine(receiver, argument)
      |""".stripMargin ->
      "ModuleDef(syntax,32771,Template(DefDef(<init>,0,List(),Empty,Empty),List(),List(),ValDef(_,8199,Empty,Empty),List(ExtMethods(List(List(TypeDef(A,259,TypeBounds(Empty,Empty,Empty))), List(ValDef(receiver,259,Ident(A),Empty))),List(DefDef(combine,129,List(List(ValDef(argument,259,Ident(A),Empty)), List(ValDef(evidence,536871171,Applied(Ident(Semigroup),List(Ident(A))),Empty))),Ident(A),Apply(Select(Ident(evidence),combine),List(Ident(receiver), Ident(argument)))))))))",
    """object operations:
      |  extension [Element](left: Element)
      |    def merge(right: Element)(using instance: Choice[Element]): Element =
      |      instance.merge(left, right)
      |""".stripMargin ->
      "ModuleDef(operations,32771,Template(DefDef(<init>,0,List(),Empty,Empty),List(),List(),ValDef(_,8199,Empty,Empty),List(ExtMethods(List(List(TypeDef(Element,259,TypeBounds(Empty,Empty,Empty))), List(ValDef(left,259,Ident(Element),Empty))),List(DefDef(merge,129,List(List(ValDef(right,259,Ident(Element),Empty)), List(ValDef(instance,536871171,Applied(Ident(Choice),List(Ident(Element))),Empty))),Ident(Element),Apply(Select(Ident(instance),merge),List(Ident(left), Ident(right)))))))))"
  )

  fixtures.zipWithIndex.foreach { case ((source, expected), index) =>
    test(s"capture U024 extension-module topology fixture $index") {
      withContext {
        val root = parseOne(source, index)
        assertEquals(structure(root), expected)
        val trees = allTrees(root)
        assertEquals(trees.size, 21)
        assertEquals(trees.map(_.source.path).distinct, Vector(""))
        assert(trees.forall(!_.source.exists))
        assert(
          trees.forall(_.span.exists),
          clues(trees.filterNot(_.span.exists).map(tree => tree.getClass.getSimpleName -> structure(tree)))
        )
        assert(trees.forall(_.symbol == NoSymbol), clues(trees.filterNot(_.symbol == NoSymbol)))
        assert(trees.forall(!_.isInstanceOf[untpd.TypedSplice]))
      }
    }
  }

  private def parseOne(source: String, index: Int)(using outer: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    given Context = outer.fresh.setReporter(reporter)
    val parsed =
      new Parsers.Parser(SourceFile.virtual(s"U024ExtensionModule$index.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.ModuleDef =>
        s"ModuleDef(${value.name},${value.mods.flags},${structure(value.impl)})"
      case value: untpd.Template =>
        s"Template(${structure(value.constr)},${value.parentsOrDerived.map(structure)},${value.derived.map(structure)},${structure(value.self)},${value.body.map(structure)})"
      case value: untpd.DefDef =>
        s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(structure))},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.ExtMethods =>
        s"ExtMethods(${value.paramss.map(_.map(structure))},${value.methods.map(structure)})"
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${value.mods.flags},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Apply =>
        s"Apply(${structure(value.fun)},${value.args.map(structure)})"
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.ModuleDef => Vector(value.impl)
      case value: untpd.Template =>
        Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body
      case value: untpd.ExtMethods =>
        value.paramss.flatten.toVector ++ value.methods.toVector
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
