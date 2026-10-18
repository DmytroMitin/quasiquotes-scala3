package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.parser.TermShape
import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

final class ExistingRootedOptionRecaptureCharacterizationTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*

  private final case class FirstRewrite(
      original: untpd.TypeDef,
      capture: Capture,
      method: MethodView,
      parameter: ParameterView,
      body: TermShape,
      result: Result
  )

  test("R0-R2 rooted Option output is recaptured while semantic projection fails at the selected constructor"):
    withContext:
      val first = firstRewrite(parseClass(fixtureSource))
      val originalMethodTree = selectedMethod(first.original)
      val originalParameterTree = originalMethodTree.paramss.head.head.asInstanceOf[untpd.ValDef]
      val firstMethodTree = selectedMethod(first.result.tree)
      val firstParameterTree = firstMethodTree.paramss.head.head.asInstanceOf[untpd.ValDef]
      val parameterSnapshot = rawSnapshot(firstParameterTree.tpt)
      val resultSnapshot = rawSnapshot(firstMethodTree.tpt)
      val secondCapture = right(ExistingClassUntypedRewrite.capture(first.result.tree))
      val secondMethod = right(secondCapture.method(secondCapture.members(1).ref))
      val secondParameter = secondMethod.parameterClauses.head.parameters.head

      assertEquals(secondCapture.members.size, 3)
      assert(secondCapture.classIdentity.sameObjectAs(new ExactIdentity(first.result.tree)))
      assert(secondCapture.exact.originalTemplate.eq(first.result.tree.rhs))
      assert(secondCapture.members.indices.forall(index =>
        secondCapture.members(index).identity.sameObjectAs(first.result.directMemberIdentities(index))
      ))
      assert(secondMethod.member == secondCapture.members(1).ref)
      assertEquals(secondMethod.parameterClauses.size, 1)
      assertEquals(secondMethod.parameterClauses.head.kind, ParameterClauseKind.Ordinary)
      assertEquals(secondMethod.parameterClauses.head.parameters.size, 1)
      assert(secondParameter.declaredType.identity.sameObjectAs(new ExactIdentity(firstParameterTree.tpt)))
      assert(secondMethod.resultType.identity.sameObjectAs(new ExactIdentity(firstMethodTree.tpt)))
      assertProjectionBoundary(secondParameter.declaredType.semantic, "parameter type")
      assertProjectionBoundary(secondMethod.resultType.semantic, "result type")
      assertCanonicalRootedPair(firstParameterTree.tpt)
      assertCanonicalRootedPair(firstMethodTree.tpt)
      assertCanonicalProvenance(firstParameterTree.tpt, originalParameterTree.tpt)
      assertCanonicalProvenance(firstMethodTree.tpt, originalMethodTree.tpt)
      assertSameSnapshot(parameterSnapshot, rawSnapshot(firstParameterTree.tpt))
      assertSameSnapshot(resultSnapshot, rawSnapshot(firstMethodTree.tpt))

      val noOp = right(ExistingClassUntypedRewrite(secondCapture, secondCapture.emptyPlan))
      assert(!noOp.changed)
      assert(noOp.tree.eq(first.result.tree))
      assert(noOp.directMemberIdentities.indices.forall(index =>
        noOp.directMemberIdentities(index).sameObjectAs(secondCapture.members(index).identity)
      ))

  test("R3 second body-only transaction requires the new capture scope and preserves both rooted Type graphs"):
    withContext:
      val first = firstRewrite(parseClass(fixtureSource))
      val firstMethodTree = selectedMethod(first.result.tree)
      val firstParameterTree = firstMethodTree.paramss.head.head.asInstanceOf[untpd.ValDef]
      val secondCapture = right(ExistingClassUntypedRewrite.capture(first.result.tree))
      val secondMethod = right(secondCapture.method(secondCapture.members(1).ref))
      val secondParameter = secondMethod.parameterClauses.head.parameters.head
      val secondBody = right(secondMethod.parameterScope.reference(secondParameter.ref))

      assertCode(
        secondCapture.emptyPlan.replaceParameterType(first.parameter.ref, TypeNormalForm.STypeIdent("String")),
        "SELECTION_FAILED"
      )
      assertCode(secondCapture.emptyPlan.omit(first.capture.members(0).ref), "SELECTION_FAILED")
      assertCode(
        secondCapture.emptyPlan.replaceResultType(first.method.ref, TypeNormalForm.STypeIdent("String")),
        "SELECTION_FAILED"
      )
      assertCode(secondCapture.emptyPlan.replaceBody(first.method.ref, secondBody), "SELECTION_FAILED")
      assertCode(
        secondCapture.emptyPlan.replaceBody(secondMethod.ref, first.body),
        "SEMANTIC_FRAGMENT_INVALID"
      )
      assertCode(ExistingClassUntypedRewrite(secondCapture, first.capture.emptyPlan), "SELECTION_FAILED")

      val plan = right(secondCapture.emptyPlan.replaceBody(secondMethod.ref, secondBody))
      val second = right(ExistingClassUntypedRewrite(secondCapture, plan))
      val secondMethodTree = selectedMethod(second.tree)
      val secondParameterTree = secondMethodTree.paramss.head.head.asInstanceOf[untpd.ValDef]

      assert(second.changed)
      assert(!second.tree.eq(first.result.tree))
      assert(!secondMethodTree.eq(firstMethodTree))
      assert(secondParameterTree.eq(firstParameterTree))
      assert(secondParameterTree.tpt.eq(firstParameterTree.tpt))
      assert(secondMethodTree.tpt.eq(firstMethodTree.tpt))
      assert(!secondMethodTree.rhs.eq(firstMethodTree.rhs))
      assert(second.directMemberIdentities(0).sameObjectAs(secondCapture.members(0).identity))
      assert(!second.directMemberIdentities(1).sameObjectAs(secondCapture.members(1).identity))
      assert(second.directMemberIdentities(2).sameObjectAs(secondCapture.members(2).identity))
      assertPreTyperClean(second.tree)

  test("R4 recaptured complex sites independently admit primitive parameter and result replacements"):
    withContext:
      val first = firstRewrite(parseClass(fixtureSource))
      val secondCapture = right(ExistingClassUntypedRewrite.capture(first.result.tree))
      val secondMethod = right(secondCapture.method(secondCapture.members(1).ref))
      val secondParameter = secondMethod.parameterClauses.head.parameters.head
      val secondBody = right(secondMethod.parameterScope.reference(secondParameter.ref))

      val parameterOnly = right(ExistingClassUntypedRewrite(
        secondCapture,
        right(secondCapture.emptyPlan.replaceParameterType(
          secondParameter.ref,
          TypeNormalForm.STypeIdent("String")
        ))
      ))
      assertDirectType(selectedParameter(parameterOnly.tree).tpt, "String")
      assertCanonicalRootedPair(selectedMethod(parameterOnly.tree).tpt)

      val resultOnly = right(ExistingClassUntypedRewrite(
        secondCapture,
        right(secondCapture.emptyPlan.replaceResultType(
          secondMethod.ref,
          TypeNormalForm.STypeIdent("String")
        ))
      ))
      assertCanonicalRootedPair(selectedParameter(resultOnly.tree).tpt)
      assertDirectType(selectedMethod(resultOnly.tree).tpt, "String")

      val combinedPlan = right(for
        p1 <- secondCapture.emptyPlan.replaceParameterType(
          secondParameter.ref,
          TypeNormalForm.STypeIdent("String")
        )
        p2 <- p1.replaceResultType(secondMethod.ref, TypeNormalForm.STypeIdent("String"))
        p3 <- p2.replaceBody(secondMethod.ref, secondBody)
      yield p3)
      val combined = right(ExistingClassUntypedRewrite(secondCapture, combinedPlan))
      assertDirectType(selectedParameter(combined.tree).tpt, "String")
      assertDirectType(selectedMethod(combined.tree).tpt, "String")
      assertPreTyperClean(combined.tree)

  test("R5 a repeated canonical replacement creates new rooted graphs at the retained outer sites"):
    withContext:
      val first = firstRewrite(parseClass(fixtureSource))
      val firstMethodTree = selectedMethod(first.result.tree)
      val firstParameterType = selectedParameter(first.result.tree).tpt
      val secondCapture = right(ExistingClassUntypedRewrite.capture(first.result.tree))
      val secondMethod = right(secondCapture.method(secondCapture.members(1).ref))
      val secondParameter = secondMethod.parameterClauses.head.parameters.head
      val secondBody = right(secondMethod.parameterScope.reference(secondParameter.ref))
      val plan = right(for
        p1 <- secondCapture.emptyPlan.replaceParameterType(secondParameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(secondMethod.ref, canonicalOptionString)
        p3 <- p2.replaceBody(secondMethod.ref, secondBody)
      yield p3)
      val second = right(ExistingClassUntypedRewrite(secondCapture, plan))
      val secondMethodTree = selectedMethod(second.tree)
      val secondParameterType = selectedParameter(second.tree).tpt

      assertFreshCanonicalGraph(secondParameterType, firstParameterType)
      assertFreshCanonicalGraph(secondMethodTree.tpt, firstMethodTree.tpt)
      assert(second.directMemberIdentities(0).sameObjectAs(secondCapture.members(0).identity))
      assert(!second.directMemberIdentities(1).sameObjectAs(secondCapture.members(1).identity))
      assert(second.directMemberIdentities(2).sameObjectAs(secondCapture.members(2).identity))
      assertPreTyperClean(second.tree)

  test("R6 two transactions match one initial plan structurally without claiming cross-transaction rollback"):
    withContext:
      val single = firstRewrite(parseClass(fixtureSource)).result
      val first = firstRewrite(parseClass(fixtureSource))
      val secondCapture = right(ExistingClassUntypedRewrite.capture(first.result.tree))
      val secondMethod = right(secondCapture.method(secondCapture.members(1).ref))
      val secondParameter = secondMethod.parameterClauses.head.parameters.head
      val secondBody = right(secondMethod.parameterScope.reference(secondParameter.ref))
      val repeatedPlan = right(for
        p1 <- secondCapture.emptyPlan.replaceParameterType(secondParameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(secondMethod.ref, canonicalOptionString)
        p3 <- p2.replaceBody(secondMethod.ref, secondBody)
      yield p3)
      val sequential = right(ExistingClassUntypedRewrite(secondCapture, repeatedPlan))

      assertEquals(selectedMethod(sequential.tree).toString, selectedMethod(single.tree).toString)
      assert(!sequential.tree.eq(single.tree))

      val beforeFailure = rawSnapshot(first.result.tree)
      val invalid = TypeNormalForm.STypeApply(
        TypeNormalForm.STypeResolved(ResolvedTypeNameId(Vector(packageOwner("scala")), "Option")),
        List(TypeNormalForm.STypeIdent("Int"))
      )
      val invalidPlan = right(for
        p1 <- secondCapture.emptyPlan.replaceParameterType(
          secondParameter.ref,
          TypeNormalForm.STypeIdent("String")
        )
        p2 <- p1.replaceResultType(secondMethod.ref, invalid)
      yield p2)
      assertCode(ExistingClassUntypedRewrite(secondCapture, invalidPlan), "UNSUPPORTED_STRUCTURE")
      assertEquals(rawSnapshot(first.result.tree), beforeFailure)

  test("second capture does not widen canonical Option replacement to unsupported sibling methods"):
    withContext:
      val cases = Vector(
        "by-name" -> "def unsupported(value: => String): String = value",
        "repeated" -> "def unsupported(value: String*): String = value.head",
        "contextual" -> "def unsupported(using value: String): String = value",
        "zero parameter" -> "def unsupported(): String = throw new RuntimeException()",
        "generic independent" -> "def unsupported[A](value: String): String = value",
        "generic dependent" -> "def unsupported[A](value: A): A = value"
      )
      cases.foreach { case (label, unsupported) =>
        val source =
          s"""class C:
             |  val before: Int = 1
             |  def echo(value: String): String = value
             |  $unsupported
             |  val after: Int = 2
             |""".stripMargin
        val first = firstRewrite(parseClass(source))
        val secondCapture = right(ExistingClassUntypedRewrite.capture(first.result.tree))
        val unsupportedMethod = right(secondCapture.method(secondCapture.members(2).ref))
        val before = rawSnapshot(first.result.tree)
        val plan = right(secondCapture.emptyPlan.replaceResultType(
          unsupportedMethod.ref,
          canonicalOptionString
        ))
        assertCode(ExistingClassUntypedRewrite(secondCapture, plan), "UNSUPPORTED_STRUCTURE", label)
        assertSameSnapshot(before, rawSnapshot(first.result.tree))
        assertCanonicalRootedPair(selectedParameter(first.result.tree).tpt)
        assertCanonicalRootedPair(selectedMethod(first.result.tree).tpt)
      }

  test("G2 remains body-only even when another direct member contains an applied Type"):
    withContext:
      val root = parseClass(
        """class G2WithAppliedSibling:
          |  val option: _root_.scala.Option[_root_.java.lang.String] = _root_.scala.None
          |  def keep[A](x: A): A = x
          |""".stripMargin
      )
      val capture = right(ExistingClassUntypedRewrite.capture(root))
      val method = right(capture.method(capture.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      assertLocalDependencyBoundary(parameter.declaredType.semantic)
      assertLocalDependencyBoundary(method.resultType.semantic)

      val typePlan = right(capture.emptyPlan.replaceParameterType(
        parameter.ref,
        TypeNormalForm.STypeIdent("String")
      ))
      assertCode(ExistingClassUntypedRewrite(capture, typePlan), "UNSUPPORTED_STRUCTURE")

      val body = right(method.parameterScope.reference(parameter.ref))
      val bodyPlan = right(capture.emptyPlan.replaceBody(method.ref, body))
      val result = right(ExistingClassUntypedRewrite(capture, bodyPlan))
      assert(result.changed)
      assert(result.directMemberIdentities(0).sameObjectAs(capture.members(0).identity))
      assert(!result.directMemberIdentities(1).sameObjectAs(capture.members(1).identity))

  test("second-capture foreign references and conflicting intents fail before owner exposure"):
    withContext:
      val first = firstRewrite(parseClass(fixtureSource))
      val secondCapture = right(ExistingClassUntypedRewrite.capture(first.result.tree))
      val secondMethod = right(secondCapture.method(secondCapture.members(1).ref))
      val secondParameter = secondMethod.parameterClauses.head.parameters.head
      val foreign = firstRewrite(parseClass(fixtureSource))
      val foreignCapture = right(ExistingClassUntypedRewrite.capture(foreign.result.tree))
      val foreignMethod = right(foreignCapture.method(foreignCapture.members(1).ref))
      val foreignParameter = foreignMethod.parameterClauses.head.parameters.head

      assertCode(
        secondCapture.emptyPlan.replaceParameterType(foreignParameter.ref, canonicalOptionString),
        "SELECTION_FAILED"
      )
      assertCode(
        secondCapture.emptyPlan.replaceResultType(foreignMethod.ref, canonicalOptionString),
        "SELECTION_FAILED"
      )
      val once = right(secondCapture.emptyPlan.replaceParameterType(
        secondParameter.ref,
        canonicalOptionString
      ))
      assertCode(once.replaceParameterType(secondParameter.ref, canonicalOptionString), "EDIT_CONFLICT")
      assertCode(once.omit(secondCapture.members(1).ref), "EDIT_CONFLICT")

  private val fixtureSource =
    """class RecaptureService:
      |  val before: Int = 1
      |  def echo(value: String): String = value
      |  val after: Int = 2
      |""".stripMargin

  private val canonicalOptionString: TypeNormalForm =
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(ResolvedTypeNameId(Vector(packageOwner("scala")), "Option")),
      List(
        TypeNormalForm.STypeResolved(
          ResolvedTypeNameId(Vector(packageOwner("java"), packageOwner("lang")), "String")
        )
      )
    )

  private def packageOwner(name: String): ResolvedTypeOwnerSegment =
    ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)

  private def firstRewrite(root: untpd.TypeDef)(using Context): FirstRewrite =
    val capture = right(ExistingClassUntypedRewrite.capture(root))
    val method = right(capture.method(capture.members(1).ref))
    val parameter = method.parameterClauses.head.parameters.head
    val body = right(method.parameterScope.reference(parameter.ref))
    val plan = right(for
      p1 <- capture.emptyPlan.replaceParameterType(parameter.ref, canonicalOptionString)
      p2 <- p1.replaceResultType(method.ref, canonicalOptionString)
      p3 <- p2.replaceBody(method.ref, body)
    yield p3)
    FirstRewrite(root, capture, method, parameter, body, right(ExistingClassUntypedRewrite(capture, plan)))

  private def selectedMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body(1).asInstanceOf[untpd.DefDef]

  private def selectedParameter(root: untpd.TypeDef)(using Context): untpd.ValDef =
    selectedMethod(root).paramss.head.head.asInstanceOf[untpd.ValDef]

  private def assertProjectionBoundary(
      result: Either[Failure, TypeNormalForm],
      path: String
  ): Unit = result match
    case Left(problem) =>
      assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
      assert(problem.detail.contains("U038 UNSUPPORTED_SELECTED_TYPE"), clues(problem))
      assert(problem.detail.contains(path), clues(problem))
    case Right(value) => fail(s"rooted $path unexpectedly projected as ${value.render}")

  private def assertLocalDependencyBoundary(result: Either[Failure, TypeNormalForm]): Unit =
    result match
      case Left(problem) =>
        assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
        assert(problem.detail.contains("LOCAL_TYPE_BINDER_DEPENDENCY_UNSUPPORTED"), clues(problem))
      case Right(value) => fail(s"local dependent Type unexpectedly projected as ${value.render}")

  private def assertCanonicalRootedPair(tree: untpd.Tree): Unit = tree match
    case untpd.AppliedTypeTree(
          untpd.Select(untpd.Select(untpd.Ident(rootOption), scalaName), optionName),
          List(
            untpd.Select(
              untpd.Select(untpd.Select(untpd.Ident(rootString), javaName), langName),
              stringName
            )
          )
        ) =>
      assertEquals(rootOption.toString, "_root_")
      assertEquals(scalaName.toString, "scala")
      assertEquals(optionName.toString, "Option")
      assertEquals(rootString.toString, "_root_")
      assertEquals(javaName.toString, "java")
      assertEquals(langName.toString, "lang")
      assertEquals(stringName.toString, "String")
    case other => fail(s"expected rooted canonical Option[String], found $other")

  private def assertCanonicalProvenance(
      tree: untpd.Tree,
      oldSite: untpd.Tree
  )(using Context): Unit =
    val nodes = allTrees(tree)
    assertEquals(nodes.size, 8)
    assertEquals(tree.source, oldSite.source)
    assertEquals(tree.span, oldSite.span)
    assertGeneratedChildren(nodes.drop(1))

  private def assertFreshCanonicalGraph(
      fresh: untpd.Tree,
      previous: untpd.Tree
  )(using Context): Unit =
    assertCanonicalRootedPair(fresh)
    assertCanonicalProvenance(fresh, previous)
    assertEquals(fresh.source, previous.source)
    assertEquals(fresh.span, previous.span)
    val freshNodes = allTrees(fresh)
    val previousNodes = allTrees(previous)
    assert(freshNodes.forall(node => previousNodes.forall(previousNode => !node.eq(previousNode))))
    assert(freshNodes(1).source.asInstanceOf[AnyRef] ne previousNodes(1).source.asInstanceOf[AnyRef])

  private def assertGeneratedChildren(children: Vector[untpd.Tree]): Unit =
    assertEquals(children.size, 7)
    val generatedSource = children.head.source
    assert(children.forall(_.source.asInstanceOf[AnyRef] eq generatedSource.asInstanceOf[AnyRef]))
    assertEquals(generatedSource.path, "generated/CanonicalResolvedOptionStringMethodType.scala")
    assertEquals(generatedSource.content.mkString, "_root_.scala.Option[_root_.java.lang.String]")
    assertEquals(
      children.map(node => (node.span.start, node.span.end, node.span.point)),
      Vector(
        (0, 19, 13),
        (0, 12, 7),
        (0, 6, 0),
        (20, 43, 37),
        (20, 36, 32),
        (20, 31, 27),
        (20, 26, 20)
      )
    )

  private def assertDirectType(tree: untpd.Tree, expected: String)(using Context): Unit =
    tree match
      case ident: untpd.Ident =>
        assertEquals(ident.name.toString, expected)
        assert(ident.source.exists)
        assert(ident.span.exists)
      case other => fail(s"expected direct $expected Type, found $other")

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    assert(allTrees(tree).forall(node =>
      node.symbol == dotty.tools.dotc.core.Symbols.NoSymbol &&
        !node.isInstanceOf[untpd.TypedSplice]
    ))

  private def rawSnapshot(tree: untpd.Tree)(using Context): Vector[(untpd.Tree, Any, Any)] =
    allTrees(tree).map(node => (node, node.source, node.span))

  private def assertSameSnapshot(
      expected: Vector[(untpd.Tree, Any, Any)],
      actual: Vector[(untpd.Tree, Any, Any)]
  ): Unit =
    assertEquals(actual.size, expected.size)
    actual.zip(expected).foreach { case ((actualTree, actualSource, actualSpan),
        (expectedTree, expectedSource, expectedSpan)) =>
      assert(actualTree.eq(expectedTree))
      assert(actualSource.asInstanceOf[AnyRef] eq expectedSource.asInstanceOf[AnyRef])
      assertEquals(actualSpan, expectedSpan)
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def assertCode[A](
      result: Either[Failure, A],
      expected: String,
      label: String = ""
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected, clues(label, problem))
    case Right(value) => fail(s"$label expected $expected, found success $value")

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U074RootedOptionRecapture.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats.collect { case root: untpd.TypeDef => root } match
          case root :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def right[A](result: Either[Failure, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
