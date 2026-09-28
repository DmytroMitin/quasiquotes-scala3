package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.{EmptyFlags, FlagSet}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.NoSource
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.types.TypeNormalForm
import quasiquotes.types.TypeNormalForm.*
import quasiquotes.types.dotty.TypeUntypedLowering

final class ExistingUntpdOrdinaryMethodTypeSlotProjectionTest
    extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}
  import ExistingUntpdOrdinaryMethodTypeSlotProjection.*

  test("projects exact one- and two-parameter slots and result slots") {
    withContext {
      val fixtures = Vector(
        (
          "class One:\n  def f(value: Int): String = value.toString\n",
          Vector[TypeNormalForm](STypeIdent("Int")),
          STypeIdent("String")
        ),
        (
          "class Two:\n  def f(left: List[Option[Int]], right: Either[String, Boolean]): (Int, String, Boolean) = ???\n",
          Vector[TypeNormalForm](
            STypeApply(STypeIdent("List"), List(STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))))),
            STypeApply(STypeIdent("Either"), List(STypeIdent("String"), STypeIdent("Boolean")))
          ),
          STypeTuple(List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean")))
        )
      )

      fixtures.foreach { (source, expectedParameters, expectedResult) =>
        val value = descriptor(source)
        assertEquals(value.parameterClauses.flatten.size, expectedParameters.size)
        value.parameterClauses.flatten.zip(expectedParameters).foreach { (parameter, expected) =>
          assertEquals(projectParameter(value, parameter), Right(expected), clues(source, parameter))
        }
        assertEquals(projectResult(value), Right(expectedResult), clues(source))
      }
    }
  }

  test("projects the complete recursive structural family and normalizes raw Parens") {
    withContext {
      val value = descriptor(
        """class Recursive:
          |  def f(
          |    tuple: (Int, String),
          |    function: (Int, String) => Boolean
          |  ): Either[List[((Int))], Option[(String, Boolean) => AnyVal]] = ???
          |""".stripMargin
      )
      val parameters = value.parameterClauses.flatten

      assertEquals(
        projectParameter(value, parameters(0)),
        Right(STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))))
      )
      assertEquals(
        projectParameter(value, parameters(1)),
        Right(STypeFunction(
          List(STypeIdent("Int"), STypeIdent("String")),
          STypeIdent("Boolean")
        ))
      )
      assertEquals(
        projectResult(value),
        Right(
          STypeApply(
            STypeIdent("Either"),
            List(
              STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
              STypeApply(
                STypeIdent("Option"),
                List(STypeFunction(
                  List(STypeIdent("String"), STypeIdent("Boolean")),
                  STypeIdent("AnyVal")
                ))
              )
            )
          )
        )
      )
    }
  }

  test("lowerable projections round-trip to expected fresh source-free topology while AnyVal stays projection-only") {
    withContext {
      val value = descriptor(
        "class Differential:\n  def f(value: AnyVal): Option[(Int, String) => Either[Boolean, List[Int]]] = ???\n"
      )
      val parameter = value.parameterClauses.head.head
      val projectedParameter = projectParameter(value, parameter).toOption.get
      val projectedResult = projectResult(value).toOption.get

      assertEquals(projectedParameter, STypeIdent("AnyVal"))
      assertEquals(
        TypeUntypedLowering.lower(projectedParameter).left.toOption.map(_.code),
        Some("UNSUPPORTED_SEMANTIC_VALUE"),
        clues(projectedParameter)
      )

      val firstRaw = TypeUntypedLowering.lower(projectedResult).fold(e => fail(e.message), identity)
      val secondRaw = TypeUntypedLowering.lower(projectedResult).fold(e => fail(e.message), identity)
      assert(!firstRaw.eq(secondRaw))
      assertEquals(
        topology(firstRaw),
        "Applied(Ident(Option),[Function([Ident(Int),Ident(String)],Applied(Ident(Either),[Ident(Boolean),Applied(Ident(List),[Ident(Int)])]))])"
      )
      allTrees(firstRaw).foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("parameter selection is exact-wrapper capture-local authority rather than spelling") {
    withContext {
      val first = descriptor("class Same:\n  def f(value: Int): String = value.toString\n")
      val second = descriptor("class Same:\n  def f(value: Int): String = value.toString\n")
      val firstParameter = first.parameterClauses.head.head
      val foreignParameter = second.parameterClauses.head.head
      val repeated = ExistingUntpdOrdinaryMethodDescriptor
        .capture(first.captured, first.memberIndex)
        .fold(problem => fail(problem.message), identity)
      val repeatedParameter = repeated.parameterClauses.head.head

      assertEquals(projectParameter(first, firstParameter), Right(STypeIdent("Int")))
      assertCode(projectParameter(first, null), "PARAMETER_REQUIRED")
      assertCode(projectParameter(first, foreignParameter), "PARAMETER_NOT_CAPTURED")
      assert(!repeatedParameter.eq(firstParameter))
      assert(repeatedParameter.tree.eq(firstParameter.tree))
      assert(repeatedParameter.tpt.eq(firstParameter.tpt))
      assertCode(projectParameter(first, repeatedParameter), "PARAMETER_NOT_CAPTURED")
      assertCode(
        projectParameter(first, firstParameter.copy(diagnosticName = "value")),
        "PARAMETER_NOT_CAPTURED"
      )
      assertCode(
        projectParameter(first, firstParameter.copy(tree = foreignParameter.tree)),
        "PARAMETER_NOT_CAPTURED"
      )
      assertCode(
        projectParameter(first, firstParameter.copy(tpt = foreignParameter.tpt)),
        "PARAMETER_NOT_CAPTURED"
      )
    }
  }

  test("unsupported opaque slots do not invalidate or mutate the U037 descriptor") {
    withContext {
      val value = descriptor(
        "class Opaque:\n  def f(value: scala.Int): Map[Int, String] = ???\n"
      )
      val before = snapshot(value.captured.originalRoot)
      val parameter = value.parameterClauses.head.head

      assertEquals(ExistingUntpdOrdinaryMethodDescriptor.validate(value), Right(()))
      assertCode(projectParameter(value, parameter), "UNSUPPORTED_SELECTED_TYPE")
      assertCode(projectResult(value), "UNSUPPORTED_APPLIED_CONSTRUCTOR")
      assertEquals(ExistingUntpdOrdinaryMethodDescriptor.validate(value), Right(()))
      assert(parameter.tpt.eq(value.method.paramss.head.head.asInstanceOf[untpd.ValDef].tpt))
      assert(value.resultType.eq(value.method.tpt))
      assertSnapshotUnchanged(before)
    }
  }

  test("unsupported identifiers constructors tuple function arities and raw families fail closed") {
    withContext {
      val fixtures = Vector(
        "class LongSlot:\n  def f(value: Long): Int = 1\n" -> ("UNSUPPORTED_IDENTIFIER", false),
        "class ListArity:\n  def f(value: List[Int, String]): Int = 1\n" -> ("UNSUPPORTED_APPLIED_CONSTRUCTOR", false),
        "class TupleArity:\n  def f(value: (Int, String, Boolean, AnyVal)): Int = 1\n" -> ("UNSUPPORTED_TUPLE_ARITY", false),
        "class FunctionArity:\n  def f(value: (Int, String, Boolean) => AnyVal): Int = 1\n" -> ("UNSUPPORTED_FUNCTION_ARITY", false),
        "class Wildcard:\n  def f(value: List[?]): Int = 1\n" -> ("UNSUPPORTED_TYPE_TOPOLOGY", false),
        "class Refinement:\n  def f(value: Int): Int { type Out = String } = ???\n" -> ("UNSUPPORTED_TYPE_TOPOLOGY", true)
      )

      fixtures.foreach { case (source, (expectedCode, resultSlot)) =>
        val value = descriptor(source)
        val actual =
          if resultSlot then projectResult(value)
          else projectParameter(value, value.parameterClauses.head.head)
        assertCode(actual, expectedCode)
        assertEquals(ExistingUntpdOrdinaryMethodDescriptor.validate(value), Right(()))
      }
    }
  }

  test("invalid stale malformed and contaminated descriptor inputs are contained before decoding") {
    withContext {
      given dotty.tools.dotc.util.SourceFile = NoSource
      val value = descriptor("class Invalid:\n  def f(value: Int): String = value.toString\n")
      val parameter = value.parameterClauses.head.head
      val symbol = newSymbol(NoSymbol, termName("contaminated"), EmptyFlags, NoType)
      val symbolic = untpd.Ident(termName("contaminated")).withType(symbol.termRef)
      val forged = Vector[Descriptor](
        null,
        value.copy(captured = null),
        value.copy(resultType = null, rhs = value.rhs),
        value.copy(resultType = untpd.TypeTree(), rhs = value.rhs),
        value.copy(resultType = parameter.tpt, rhs = value.rhs),
        value.copy(resultType = symbolic, rhs = value.rhs),
        value.copy(resultType = untpd.TypedSplice(symbolic), rhs = value.rhs),
        value.copy(
          parameterClauses = Vector(Vector(parameter, parameter)),
          resultType = value.resultType,
          rhs = value.rhs
        )
      )

      forged.foreach { descriptor =>
        assertCode(projectResult(descriptor), "INVALID_DESCRIPTOR")
      }
      assertCode(projectParameter(null, parameter), "INVALID_DESCRIPTOR")
    }
  }

  test("repeated projection returns equal fresh semantic data and leaves the complete original graph unchanged") {
    withContext {
      val value = descriptor(
        "class Immutable:\n  def f(value: Either[List[Int], Option[String]]): (Int, String) = ???\n"
      )
      val parameter = value.parameterClauses.head.head
      val before = snapshot(value.captured.originalRoot)
      val firstParameter = projectParameter(value, parameter).toOption.get
      val secondParameter = projectParameter(value, parameter).toOption.get
      val firstResult = projectResult(value).toOption.get
      val secondResult = projectResult(value).toOption.get

      assertEquals(firstParameter, secondParameter)
      assertEquals(firstResult, secondResult)
      assert(!firstParameter.asInstanceOf[AnyRef].eq(secondParameter.asInstanceOf[AnyRef]))
      assert(!firstResult.asInstanceOf[AnyRef].eq(secondResult.asInstanceOf[AnyRef]))
      assertSnapshotUnchanged(before)
    }
  }

  private def descriptor(source: String)(using Context): Descriptor =
    val root = parseSingleTypeDef(source)
    val captured = ExistingUntpdClassMemberFilter.capture(root)
      .fold(problem => fail(problem.message), identity)
    ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0)
      .fold(problem => fail(problem.message), identity)

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("TypeSlotProjection.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def assertCode[A](value: Either[Error, A], expected: String): Unit =
    value match
      case Left(error) =>
        assertEquals(error.code, expected, clues(error))
        assert(error.detail.nonEmpty, clues(error))
        assert(error.message.nonEmpty, clues(error))
      case Right(actual) => fail(s"expected $expected, obtained $actual")

  private def topology(tree: untpd.Tree): String = tree match
    case untpd.Ident(name) => s"Ident($name)"
    case untpd.AppliedTypeTree(constructor, arguments) =>
      s"Applied(${topology(constructor)},[${arguments.map(topology).mkString(",")}])"
    case untpd.Tuple(elements) => s"Tuple([${elements.map(topology).mkString(",")}])"
    case untpd.Function(arguments, result) =>
      s"Function([${arguments.map(topology).mkString(",")}],${topology(result)})"
    case other => s"Unexpected(${other.getClass.getName})"

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] = tree match
    case untpd.AppliedTypeTree(constructor, arguments) =>
      tree :: allTrees(constructor) ::: arguments.flatMap(allTrees)
    case untpd.Tuple(elements) => tree :: elements.flatMap(allTrees)
    case untpd.Function(arguments, result) =>
      tree :: arguments.flatMap(allTrees) ::: allTrees(result)
    case _ => tree :: Nil

  private final case class TreeSnapshot(
      tree: untpd.Tree,
      source: dotty.tools.dotc.util.SourceFile,
      span: Span,
      symbol: Symbol,
      modifiers: Option[FlagSet]
  )

  private def snapshot(tree: untpd.Tree)(using Context): Vector[TreeSnapshot] =
    ExistingUntpdClassMemberFilter.allTrees(tree).map { node =>
      val modifiers = node match
        case member: untpd.MemberDef => Some(member.mods.flags)
        case _ => None
      TreeSnapshot(node, node.source, node.span, node.symbol, modifiers)
    }

  private def assertSnapshotUnchanged(before: Vector[TreeSnapshot])(using Context): Unit =
    val after = ExistingUntpdClassMemberFilter.allTrees(before.head.tree)
    assertEquals(after.size, before.size)
    after.zip(before).foreach { (actual, expected) =>
      assert(actual.eq(expected.tree))
      assert(actual.source.eq(expected.source))
      assertEquals(actual.span, expected.span)
      assertEquals(actual.symbol, expected.symbol)
      expected.modifiers.foreach(flags =>
        assertEquals(actual.asInstanceOf[untpd.MemberDef].mods.flags, flags)
      )
    }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
