package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.dotty.BoundedExtensionModulePlan.*
import quasiquotes.parser.BinderId

class BoundedExtensionModuleGeneratedOriginAdapterTest extends munit.FunSuite:
  test("positions every canonical and renamed extension-module node in deterministic generated source") {
    withContext {
      Vector(
        Names("syntax", "A", "receiver", "combine", "argument", "evidence", "Semigroup"),
        Names("operations", "Element", "left", "merge", "right", "instance", "Choice")
      ).zipWithIndex.foreach { case (names, index) =>
        val result = BoundedExtensionModuleGeneratedOriginAdapter
          .lower(validPlan(names), s"<quasiquotes-generated:u024-$index>")
          .fold(problem => fail(problem.message), identity)
        assertEquals(result.generatedSource, names.source)
        assertEquals(result.virtualSourceName, s"<quasiquotes-generated:u024-$index>")
        val trees = BoundedExtensionModuleUntypedLowerer.allTrees(result.tree)
        assertEquals(trees.size, 21)
        trees.foreach { tree =>
          assert(tree.source.exists, clues(tree.getClass.getSimpleName))
          assertEquals(tree.source.path, result.virtualSourceName)
          assertEquals(tree.source.content.mkString, names.source)
          assert(tree.span.exists, clues(tree.getClass.getSimpleName))
          assert(tree.span.start >= result.tree.span.start)
          assert(tree.span.end <= result.tree.span.end)
          assert(tree.span.start <= tree.span.point)
          assert(tree.span.point <= tree.span.end)
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }
  }

  test("rejects invalid generated origin without reparsing or partial positioning") {
    withContext {
      val plan = validPlan(
        Names("syntax", "A", "receiver", "combine", "argument", "evidence", "Semigroup")
      )
      assertEquals(
        BoundedExtensionModuleGeneratedOriginAdapter
          .lower(plan, null)
          .left.toOption.map(_.code),
        Some("GENERATED_ORIGIN_INVALID")
      )
      assertEquals(
        BoundedExtensionModuleGeneratedOriginAdapter
          .lower(plan, " ordinary.scala")
          .left.toOption.map(_.code),
        Some("GENERATED_ORIGIN_INVALID")
      )
    }
  }

  private final case class Names(
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      argument: String,
      evidence: String,
      evidenceType: String
  ):
    val source =
      s"""object $module:
         |  extension [$typeParameter]($receiver: $typeParameter)
         |    def $method($argument: $typeParameter)(using $evidence: $evidenceType[$typeParameter]): $typeParameter =
         |      $evidence.$method($receiver, $argument)
         |""".stripMargin

  private def validPlan(names: Names): Plan =
    val typeBinder = BinderId(0)
    val receiverBinder = BinderId(1)
    val argumentBinder = BinderId(2)
    val evidenceBinder = BinderId(3)
    val reference = TypeParameterReference(typeBinder, names.typeParameter)
    BoundedExtensionModulePlan
      .create(
        names.module,
        names.method,
        TypeParameter(typeBinder, names.typeParameter),
        ReceiverParameter(receiverBinder, names.receiver, reference),
        OrdinaryArgument(argumentBinder, names.argument, reference),
        ContextualParameter(
          evidenceBinder,
          names.evidence,
          Applied(SourceName(names.evidenceType), Vector(reference))
        ),
        reference,
        DelegatedBody(
          BodyTermReference(evidenceBinder),
          names.method,
          Vector(
            BodyTermReference(receiverBinder),
            BodyTermReference(argumentBinder)
          )
        )
      )
      .fold(problem => fail(problem.message), identity)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
