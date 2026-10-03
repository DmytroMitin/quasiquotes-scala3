package com.github.dmytromitin.auxify.macros.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.ExtensionModulePeerBridge

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class ExtensionModulePeerBridgeTest extends munit.FunSuite:
  test("foreign AUXify package receives canonical and renamed insertion-ready ModuleDefs") {
    withContext {
      val rows = Vector(
        Names(CanonicalSource, "syntax", "A", "a", "combine", "a1", "inst", "Monoid", "AuxifyGeneratedSyntax.scala"),
        Names(RenamedSource, "operations", "Element", "left", "merge", "right", "evidence", "Combine", "AuxifyGeneratedOperations.scala")
      )

      rows.foreach { names =>
        val definition = parse(names.source)
        val before = definition.structure
        val lowered: ExtensionModulePeerBridge.Lowered =
          lower(definition, names.virtualSourceName)

        assertEquals(definition.structure, before)
        assertEquals(lowered.generatedSource, names.source)
        assertEquals(lowered.virtualSourceName, names.virtualSourceName)
        assertExactShape(lowered.tree, names)
        assertPositionedGraph(lowered.tree, names.source, names.virtualSourceName)
      }
    }
  }

  test("repeated public calls preserve input and return fresh deterministic graphs") {
    withContext {
      val definition = parse(CanonicalSource)
      val before = definition.structure
      val first = lower(definition, "AuxifyGeneratedSyntax.scala")
      val second = lower(definition, "AuxifyGeneratedSyntax.scala")
      val firstTrees = allTrees(first.tree)
      val secondTrees = allTrees(second.tree)

      assertEquals(definition.structure, before)
      assert(!(first.tree eq second.tree))
      assert(!(first.tree.source eq second.tree.source))
      assertEquals(firstTrees.size, 21)
      assertEquals(secondTrees.size, 21)
      assert(firstTrees.zip(secondTrees).forall { case (left, right) => !(left eq right) })
      assertEquals(first.generatedSource, second.generatedSource)
      assertEquals(rawStructure(first.tree), rawStructure(second.tree))
    }
  }

  test("public boundary classifies missing and unsupported structural topology") {
    withContext {
      assertFailure(null, "Generated.scala", "INVALID_SCALAMETA_DEFINITION")

      val sourceRows = Vector(
        "private " + CanonicalSource,
        CanonicalSource.replace("object syntax:", "object syntax extends Parent:"),
        CanonicalSource + "  val extra = 1\n",
        replaceFirstLiteral(CanonicalSource, "[A]", "[A, B]"),
        replaceFirstLiteral(CanonicalSource, "[A]", "[A <: Any]"),
        CanonicalSource.replace("(a: A)", "(a: A = ???)"),
        CanonicalSource.replace("(a: A)", "(using a: A)"),
        CanonicalSource.replace("(a: A)", "(a: A)(other: A)"),
        CanonicalSource.replace("def combine", "private def combine"),
        CanonicalSource.replace("def combine", "def combine[B]"),
        CanonicalSource.replace("(a1: A)(using inst: Monoid[A])", "(a1: A)"),
        CanonicalSource.replace("(a1: A)(using inst: Monoid[A])", "(using inst: Monoid[A])(a1: A)"),
        CanonicalSource.replace("(using inst: Monoid[A])", "(inst: Monoid[A])"),
        CanonicalSource.replace("(a1: A)", "(a1: A, a2: A)"),
        CanonicalSource.replace("(using inst: Monoid[A])", "(using inst: Monoid[A], other: Monoid[A])")
      )
      sourceRows.foreach(source =>
        assertFailure(parse(source), "Generated.scala", "UNSUPPORTED_EXTENSION_MODULE_TOPOLOGY")
      )

      val canonical = parse(CanonicalSource)
      val extension = canonical.templ.stats.head.asInstanceOf[Defn.ExtensionGroup]
      val astRows = Vector(
        canonical.copy(templ = canonical.templ.copy(stats = Nil)),
        canonical.copy(templ = canonical.templ.copy(stats = List(q"val extra: Int = 1"))),
        canonical.copy(templ = canonical.templ.copy(self = Self(Term.Name("self"), None))),
        canonical.copy(templ = canonical.templ.copy(derives = List(Type.Name("Derived")))),
        withExtension(canonical, Defn.ExtensionGroup(None, extension.body))
      )
      astRows.foreach(definition =>
        assertFailure(definition, "Generated.scala", "UNSUPPORTED_EXTENSION_MODULE_TOPOLOGY")
      )
    }
  }

  test("public boundary classifies invalid name Type and Term roles") {
    withContext {
      val nameRows = Vector(
        CanonicalSource.replace("a1: A", "a: A").replace("inst.combine(a, a1)", "inst.combine(a, a)"),
        CanonicalSource.replace("Monoid[A]", "A[A]"),
        CanonicalSource.replace("object syntax", "object `type`")
      )
      nameRows.foreach(source =>
        assertFailure(parse(source), "Generated.scala", "INVALID_EXTENSION_MODULE_NAME_ROLE")
      )

      val typeRows = Vector(
        CanonicalSource.replace("(a: A)", "(a: String)"),
        CanonicalSource.replace("a1: A", "a1: String"),
        CanonicalSource.replace("inst: Monoid[A]", "inst: Monoid"),
        CanonicalSource.replace("Monoid[A]", "Monoid[A, A]"),
        CanonicalSource.replace("Monoid[A]", "Monoid[String]"),
        CanonicalSource.replace("): A =\n", "): String =\n")
      )
      typeRows.foreach(source =>
        assertFailure(parse(source), "Generated.scala", "INVALID_EXTENSION_MODULE_TYPE_ROLE")
      )

      val termRows = Vector(
        CanonicalSource.replace("inst.combine(a, a1)", "inst"),
        CanonicalSource.replace("inst.combine(a, a1)", "combine(a, a1)"),
        CanonicalSource.replace("inst.combine(a, a1)", "other.combine(a, a1)"),
        CanonicalSource.replace("inst.combine(a, a1)", "inst.other(a, a1)"),
        CanonicalSource.replace("inst.combine(a, a1)", "inst.combine(a)"),
        CanonicalSource.replace("inst.combine(a, a1)", "inst.combine(a, a1, a)"),
        CanonicalSource.replace("inst.combine(a, a1)", "inst.combine(a1, a)")
      )
      termRows.foreach(source =>
        assertFailure(parse(source), "Generated.scala", "INVALID_EXTENSION_MODULE_TERM_ROLE")
      )
    }
  }

  test("public boundary rejects invalid virtual sources before returning raw output") {
    withContext {
      val canonical = parse(CanonicalSource)
      assertFailure(canonical, null, "INVALID_VIRTUAL_SOURCE_NAME")
      assertFailure(canonical, "Bad\nName.scala", "INVALID_VIRTUAL_SOURCE_NAME")
      assertFailure(canonical, " ordinary.scala", "INVALID_VIRTUAL_SOURCE_NAME")
    }
  }

  private val CanonicalSource =
    """object syntax:
      |  extension [A](a: A)
      |    def combine(a1: A)(using inst: Monoid[A]): A =
      |      inst.combine(a, a1)
      |""".stripMargin

  private val RenamedSource =
    """object operations:
      |  extension [Element](left: Element)
      |    def merge(right: Element)(using evidence: Combine[Element]): Element =
      |      evidence.merge(left, right)
      |""".stripMargin

  private final case class Names(
      source: String,
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      argument: String,
      evidence: String,
      evidenceType: String,
      virtualSourceName: String
  )

  private def lower(definition: Defn.Object, virtualSourceName: String)(using Context): ExtensionModulePeerBridge.Lowered =
    ExtensionModulePeerBridge
      .lower(definition, virtualSourceName)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def assertFailure(definition: Defn.Object, virtualSourceName: String, expectedCode: String)(using Context): Unit =
    val result = ExtensionModulePeerBridge.lower(definition, virtualSourceName)
    val failure = result.left.toOption.getOrElse(fail(s"malformed extension module unexpectedly lowered: $result"))
    assertEquals(failure.code, expectedCode, clues(failure))
    assert(failure.detail.nonEmpty, clues(failure))

  private def assertExactShape(module: untpd.ModuleDef, names: Names)(using Context): Unit =
    assertEquals(module.name.toString, names.module)
    assertEquals(module.mods.flags, Flags.Module)
    module.impl.body match
      case List(extension: untpd.ExtMethods) =>
        extension.paramss match
          case List(List(tparam: untpd.TypeDef), List(receiver: untpd.ValDef)) =>
            assertEquals(tparam.name.toString, names.typeParameter)
            assertEquals(tparam.mods.flags, Flags.Param)
            assertEquals(receiver.name.toString, names.receiver)
            assertEquals(receiver.mods.flags, Flags.Param)
            assertIdent(receiver.tpt, names.typeParameter)
          case other => fail(s"expected one Type parameter and receiver, found $other")
        extension.methods match
          case List(method: untpd.DefDef) =>
            assertEquals(method.name.toString, names.method)
            assertEquals(method.mods.flags, Flags.Method)
            method.paramss match
              case List(List(argument: untpd.ValDef), List(evidence: untpd.ValDef)) =>
                assertEquals(argument.name.toString, names.argument)
                assertEquals(argument.mods.flags, Flags.Param)
                assertIdent(argument.tpt, names.typeParameter)
                assertEquals(evidence.name.toString, names.evidence)
                assertEquals(evidence.mods.flags, Flags.Param | Flags.Given)
                evidence.tpt match
                  case untpd.AppliedTypeTree(untpd.Ident(constructor), List(untpd.Ident(argumentType))) =>
                    assertEquals(constructor.toString, names.evidenceType)
                    assertEquals(argumentType.toString, names.typeParameter)
                  case other => fail(s"expected unary evidence Type, found $other")
              case other => fail(s"expected ordinary then contextual clauses, found $other")
            assertIdent(method.tpt, names.typeParameter)
            method.rhs match
              case untpd.Apply(untpd.Select(untpd.Ident(evidence), selected), List(untpd.Ident(receiver), untpd.Ident(argument))) =>
                assertEquals(evidence.toString, names.evidence)
                assertEquals(selected.toString, names.method)
                assertEquals(receiver.toString, names.receiver)
                assertEquals(argument.toString, names.argument)
              case other => fail(s"expected delegated two-argument body, found $other")
          case other => fail(s"expected one extension method, found $other")
      case other => fail(s"expected one ExtMethods body, found $other")

  private def assertPositionedGraph(module: untpd.ModuleDef, source: String, virtualSourceName: String)(using Context): Unit =
    val trees = allTrees(module)
    assertEquals(trees.size, 21)
    assert(trees.forall(tree => tree.source eq module.source))
    trees.foreach { tree =>
      assert(tree.source.exists, clues(tree))
      assertEquals(tree.source.path, virtualSourceName, clues(tree))
      assertEquals(tree.source.content.mkString, source, clues(tree))
      assert(tree.span.exists, clues(tree))
      assert(tree.span.start >= 0, clues(tree))
      assert(tree.span.start <= tree.span.point, clues(tree))
      assert(tree.span.point <= tree.span.end, clues(tree))
      assert(tree.span.end <= source.length, clues(tree))
      assertEquals(tree.symbol, NoSymbol, clues(tree))
      assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree))
      val children = directChildren(tree).filterNot(_.isEmpty)
      children.foreach { child =>
        assert(child.span.start >= tree.span.start, clues(child))
        assert(child.span.end <= tree.span.end, clues(child))
      }
      children.zip(children.drop(1)).foreach { case (left, right) =>
        assert(left.span.end <= right.span.start, clues(left, right))
      }
    }

  private def assertIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Ident(name) => assertEquals(name.toString, expected)
      case other => fail(s"expected Ident($expected), found $other")

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.ModuleDef => Vector(value.impl)
      case value: untpd.Template => Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++ Vector(value.self) ++ value.body
      case value: untpd.ExtMethods => value.paramss.flatten.toVector ++ value.methods.toVector
      case value: untpd.DefDef => value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def rawStructure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.ModuleDef => s"ModuleDef(${value.name},${value.mods.flags},${rawStructure(value.impl)})"
      case value: untpd.Template => s"Template(${rawStructure(value.constr)},${value.parentsOrDerived.map(rawStructure)},${value.derived.map(rawStructure)},${rawStructure(value.self)},${value.body.map(rawStructure)})"
      case value: untpd.ExtMethods => s"ExtMethods(${value.paramss.map(_.map(rawStructure))},${value.methods.map(rawStructure)})"
      case value: untpd.DefDef => s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(rawStructure))},${rawStructure(value.tpt)},${rawStructure(value.rhs)})"
      case value: untpd.TypeDef => s"TypeDef(${value.name},${value.mods.flags},${rawStructure(value.rhs)})"
      case value: untpd.ValDef => s"ValDef(${value.name},${value.mods.flags},${rawStructure(value.tpt)},${rawStructure(value.rhs)})"
      case value: untpd.TypeBoundsTree => s"TypeBounds(${rawStructure(value.lo)},${rawStructure(value.hi)},${rawStructure(value.alias)})"
      case value: untpd.AppliedTypeTree => s"Applied(${rawStructure(value.tpt)},${value.args.map(rawStructure)})"
      case value: untpd.Apply => s"Apply(${rawStructure(value.fun)},${value.args.map(rawStructure)})"
      case value: untpd.Select => s"Select(${rawStructure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def withExtension(definition: Defn.Object, extension: Defn.ExtensionGroup): Defn.Object =
    definition.copy(templ = definition.templ.copy(stats = List(extension)))

  private def replaceFirstLiteral(source: String, from: String, to: String): String =
    val index = source.indexOf(from)
    require(index >= 0, s"missing literal $from")
    source.substring(0, index) + to + source.substring(index + from.length)

  private def parse(source: String): Defn.Object =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Object => definition
      case other => fail(s"expected Defn.Object, found ${other.productPrefix}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
