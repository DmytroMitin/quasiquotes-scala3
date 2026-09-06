package external.consumer

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.*
import quasiquotes.definitions.dotty.DefinitionUntypedLowering
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

import scala.compiletime.testing.typeCheckErrors

final class DefinitionUntypedLoweringTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")
  private val stringType = TypeNormalForm.STypeIdent("String")

  test("foreign consumer lowers all five public semantic Definition families"):
    withContext:
      val fixtures = List[(SemanticDefinition, Class[? <: untpd.MemberDef])](
        immutableValue -> classOf[untpd.ValDef],
        parameterlessMethod -> classOf[untpd.DefDef],
        oneParameterMethod -> classOf[untpd.DefDef],
        twoParameterMethod -> classOf[untpd.DefDef],
        typeAlias -> classOf[untpd.TypeDef]
      )

      fixtures.foreach { case (semantic, expectedClass) =>
        val raw = lower(semantic)
        assert(expectedClass.isInstance(raw), clues(semantic, raw))
        assertSourceFree(raw)
      }

      assertEquals(lower(immutableValue).name.toString, "answer")
      assertEquals(lower(parameterlessMethod).name.toString, "answer")
      assertEquals(lower(oneParameterMethod).name.toString, "show")
      assertEquals(lower(twoParameterMethod).name.toString, "choose")
      assertEquals(lower(typeAlias).name.toString, "T")

  test("method bodies lower from the persistent public parameter scope"):
    withContext:
      lower(oneParameterMethod) match
        case method: untpd.DefDef =>
          assertEquals(method.paramss.map(_.map(_.name.toString)), List(List("x")))
          method.rhs match
            case untpd.Select(untpd.Ident(parameter), selected) =>
              assertEquals(parameter.toString, "x")
              assertEquals(selected.toString, "toString")
            case other => fail(s"expected x.toString, found $other")
        case other => fail(s"expected DefDef, found $other")

      lower(twoParameterMethod) match
        case method: untpd.DefDef =>
          assertEquals(method.paramss.map(_.map(_.name.toString)), List(List("x", "y")))
          assertEquals(method.rhs.asInstanceOf[untpd.Ident].name.toString, "x")
        case other => fail(s"expected DefDef, found $other")

      lower(swappedPairMethod) match
        case method: untpd.DefDef =>
          method.rhs match
            case untpd.Tuple(List(untpd.Ident(first), untpd.Ident(second))) =>
              assertEquals(first.toString, "right")
              assertEquals(second.toString, "left")
            case other => fail(s"expected binder-correlated tuple, found $other")
        case other => fail(s"expected DefDef, found $other")

  test("nested admitted Type forms retain exact raw topology"):
    withContext:
      val nested = right(
        SemanticDefinition.typeAlias(
          name("Nested"),
          TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent("Option"),
            List(
              TypeNormalForm.STypeTuple(
                List(intType, TypeNormalForm.STypeFunction(List(stringType), intType))
              )
            )
          )
        )
      )

      lower(nested) match
        case alias: untpd.TypeDef =>
          assertEquals(
            topology(alias.rhs),
            "Applied(Ident(Option),[Tuple([Ident(Int),Function([Ident(String)],Ident(Int))])])"
          )
        case other => fail(s"expected TypeDef, found $other")

  test("each lowering call returns a wholly fresh raw member graph"):
    withContext:
      val semantic = twoParameterMethod
      val first = allTrees(lower(semantic))
      val second = allTrees(lower(semantic))

      assertEquals(first.size, second.size)
      first.zip(second).foreach { case (left, right) =>
        assert(!(left eq right), clues(left, right))
      }

  test("foreign consumer receives the stable missing-input code"):
    withContext:
      val failure = DefinitionUntypedLowering
        .lower(null)
        .left
        .toOption
        .getOrElse(fail("null semantic Definition unexpectedly lowered"))

      assertEquals(failure.code, "MISSING_INPUT")
      assert(failure.detail.nonEmpty)
      assertEquals(failure.message, s"${failure.code}: ${failure.detail}")

  test("private Definition carriers and lowerers remain inaccessible"):
    assert(
      typeCheckErrors(
        "quasiquotes.definitions.SemanticDefinitionShapeAdapter.adapt(null)"
      ).nonEmpty
    )
    assert(typeCheckErrors("classOf[quasiquotes.definitions.DefinitionShape]").nonEmpty)
    assert(typeCheckErrors("quasiquotes.parser.BinderId(0)").nonEmpty)
    assert(
      typeCheckErrors(
        "quasiquotes.definitions.dotty.DefinitionShapeUntypedLowerer.lower(null)"
      ).nonEmpty
    )

  private def immutableValue: SemanticDefinition =
    right(
      SemanticDefinition.immutableValue(
        name("answer"),
        intType,
        TermShape.Literal("42")
      )
    )

  private def parameterlessMethod: SemanticDefinition =
    right(
      SemanticDefinition.concreteMethod(name("answer"), Vector.empty, intType)(
        _ => Right(TermShape.Literal("42"))
      )
    )

  private def oneParameterMethod: SemanticDefinition =
    right(
      SemanticDefinition.concreteMethod(
        name("show"),
        Vector(clause(parameter("x", intType))),
        stringType
      )(_.reference(0, 0).map(reference => TermShape.Select(reference, "toString")))
    )

  private def twoParameterMethod: SemanticDefinition =
    right(
      SemanticDefinition.concreteMethod(
        name("choose"),
        Vector(clause(parameter("x", intType), parameter("y", intType))),
        intType
      )(_.reference(0, 0))
    )

  private def swappedPairMethod: SemanticDefinition =
    right(
      SemanticDefinition.concreteMethod(
        name("swapped"),
        Vector(clause(parameter("left", intType), parameter("right", intType))),
        TypeNormalForm.STypeTuple(List(intType, intType))
      ) { scope =>
        for
          left <- scope.reference(0, 0)
          right <- scope.reference(0, 1)
        yield TermShape.Tuple(List(right, left))
      }
    )

  private def typeAlias: SemanticDefinition =
    right(SemanticDefinition.typeAlias(name("T"), intType))

  private def name(source: String): DefinitionName =
    right(DefinitionName.fromSource(source))

  private def parameter(
      source: String,
      declaredType: TypeNormalForm
  ): DefinitionParameter =
    DefinitionParameter(name(source), declaredType)

  private def clause(
      parameters: DefinitionParameter*
  ): DefinitionParameterClause =
    right(DefinitionParameterClause.ordinary(parameters.toVector))

  private def right[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def lower(
      definition: SemanticDefinition
  )(using Context): untpd.MemberDef =
    DefinitionUntypedLowering
      .lower(definition)
      .fold(problem => fail(problem.message), identity)

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    allTrees(tree).foreach { node =>
      assert(!node.source.exists, clues(node))
      assert(!node.span.exists, clues(node))
      assertEquals(node.symbol, NoSymbol, clues(node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(node))
    }

  private def topology(tree: untpd.Tree)(using Context): String =
    tree match
      case untpd.Ident(name) => s"Ident($name)"
      case untpd.AppliedTypeTree(constructor, arguments) =>
        s"Applied(${topology(constructor)},[${arguments.map(topology).mkString(",")}])"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(topology).mkString(",")}])"
      case untpd.Function(arguments, result) =>
        s"Function([${arguments.map(topology).mkString(",")}],${topology(result)})"
      case other => s"Unexpected(${other.getClass.getName})"

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: (tree match
      case value: untpd.TypeDef => allTrees(value.rhs)
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector.flatMap(allTrees) ++
          allTrees(value.tpt) ++ allTrees(value.rhs)
      case value: untpd.ValDef =>
        Vector(value.tpt, value.rhs).filterNot(_.isEmpty).flatMap(allTrees)
      case value: untpd.Select => allTrees(value.qualifier)
      case value: untpd.Apply => allTrees(value.fun) ++ value.args.toVector.flatMap(allTrees)
      case value: untpd.Typed => allTrees(value.expr) ++ allTrees(value.tpt)
      case value: untpd.AppliedTypeTree => allTrees(value.tpt) ++ value.args.toVector.flatMap(allTrees)
      case value: untpd.Tuple => value.trees.toVector.flatMap(allTrees)
      case value: untpd.Function => value.args.toVector.flatMap(allTrees) ++ allTrees(value.body)
      case _ => Vector.empty)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
