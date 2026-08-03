package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

class ConstructedDefinitionGeneratedOriginPreflightTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      expectedName: String,
      expectedMethod: Boolean
  )

  private val fixtures = Vector(
    Fixture("def value: Int = 1", "value", expectedMethod = true),
    Fixture("val value: String = \"text\"", "value", expectedMethod = false),
    Fixture(
      "def `type`: List[String] = if true then \"yes\" else \"no\"",
      "type",
      expectedMethod = true
    ),
    Fixture(
      "val `val`: Option[Int] = (1: Int)",
      "val",
      expectedMethod = false
    )
  )

  fixtures.foreach { fixture =>
    test(s"records complete parsed generated-definition position shape: ${fixture.source}") {
      val base = new ContextBase
      val reporter = new StoreReporter(null)
      given Context = base.initialCtx.fresh.setReporter(reporter)
      val parsed =
        new Parser(
          SourceFile.virtual(
            "ConstructedDefinitionGeneratedOriginPreflight.scala",
            fixture.source
          )
        ).parse()

      assertEquals(reporter.pendingMessages.toList, Nil)
      val definition =
        parsed match
          case packageDef: untpd.PackageDef =>
            assertEquals(packageDef.stats.size, 1)
            packageDef.stats.head
          case other =>
            fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

      val (name, definitionType, body, isMethod) =
        definition match
          case method: untpd.DefDef =>
            assertEquals(method.paramss, Nil)
            assertEquals(method.mods.flags, Flags.Method)
            (method.name.toString, method.tpt, method.rhs, true)
          case value: untpd.ValDef =>
            assert(!value.mods.hasFlags)
            (value.name.toString, value.tpt, value.rhs, false)
          case other =>
            fail(s"expected DefDef or ValDef, found ${other.getClass.getSimpleName}")

      val typeTrees = allTrees(definitionType)
      val bodyTrees = allTrees(body)
      val definitionChildren = directChildren(definition)

      println(
        s"GENERATED_DEFINITION_PREFLIGHT source=${fixture.source} length=${fixture.source.length}"
      )
      println(
        s"GENERATED_DEFINITION_PREFLIGHT root=${spanSummary(definition)} name=$name method=$isMethod childCount=${definitionChildren.size}"
      )
      println(
        s"GENERATED_DEFINITION_PREFLIGHT type=${typeTrees.map(treeSummary).mkString("[", ",", "]")}"
      )
      println(
        s"GENERATED_DEFINITION_PREFLIGHT body=${bodyTrees.map(treeSummary).mkString("[", ",", "]")}"
      )

      assertEquals(name, fixture.expectedName)
      assertEquals(isMethod, fixture.expectedMethod)
      assertEquals(definition.span.start, 0)
      assertEquals(definition.span.end, fixture.source.length)
      assert(definition.span.point >= definition.span.start)
      assert(definition.span.point <= definition.span.end)
      assertEquals(definitionChildren, Vector(definitionType, body))
      assert(definitionType.span.end <= body.span.start)
      (typeTrees ++ bodyTrees).foreach { tree =>
        assert(tree.span.exists, clues(treeSummary(tree)))
        assert(tree.span.start >= definition.span.start)
        assert(tree.span.end <= definition.span.end)
      }
      assert(
        !allTrees(definition).exists {
          case ident: untpd.Ident => ident.name.toString == name
          case _ => false
        },
        clues("the definition name is a field rather than a child Ident")
      )
    }
  }

  private def treeSummary(tree: untpd.Tree): String =
    s"${tree.getClass.getSimpleName}${spanSummary(tree)}"

  private def spanSummary(tree: untpd.Tree): String =
    s"(${tree.span.start},${tree.span.end},${tree.span.point})"

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.tpt +: value.rhs +: Vector.empty
      case value: untpd.ValDef =>
        Vector(value.tpt, value.rhs)
      case value: untpd.Select =>
        Vector(value.qualifier)
      case value: untpd.Apply =>
        value.fun +: value.args.toVector
      case value: untpd.InfixOp =>
        Vector(value.left, value.op, value.right)
      case value: untpd.PrefixOp =>
        Vector(value.op, value.od)
      case value: untpd.Typed =>
        Vector(value.expr, value.tpt)
      case value: untpd.AppliedTypeTree =>
        value.tpt +: value.args.toVector
      case value: untpd.Tuple =>
        value.trees.toVector
      case value: untpd.Function =>
        value.args.toVector :+ value.body
      case value: untpd.If =>
        Vector(value.cond, value.thenp, value.elsep)
      case value: untpd.Parens =>
        Vector(value.t)
      case _ =>
        Vector.empty
