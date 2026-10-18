package external.consumer

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

final class ExistingClassUntypedRewriteRootedOptionRecaptureTest extends munit.FunSuite:
  test("foreign package recaptures rooted output and performs bounded second public transactions"):
    withContext:
      val root = parseClass(fixtureSource)
      val firstCapture = right(ExistingClassUntypedRewrite.capture(root))
      val firstMethod = right(firstCapture.method(firstCapture.members(1).ref))
      val firstParameter = firstMethod.parameterClauses.head.parameters.head
      val firstBody = right(firstMethod.parameterScope.reference(firstParameter.ref))
      val firstPlan = right(for
        p1 <- firstCapture.emptyPlan.replaceParameterType(firstParameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(firstMethod.ref, canonicalOptionString)
        p3 <- p2.replaceBody(firstMethod.ref, firstBody)
      yield p3)
      val first = right(ExistingClassUntypedRewrite(firstCapture, firstPlan))

      val secondCapture = right(ExistingClassUntypedRewrite.capture(first.tree))
      val secondMethod = right(secondCapture.method(secondCapture.members(1).ref))
      val secondParameter = secondMethod.parameterClauses.head.parameters.head
      val secondBody = right(secondMethod.parameterScope.reference(secondParameter.ref))

      assertEquals(secondCapture.members.size, 3)
      assert(secondCapture.members.indices.forall(index =>
        secondCapture.members(index).identity.sameObjectAs(first.directMemberIdentities(index))
      ))
      assertProjectionBoundary(secondParameter.declaredType.semantic)
      assertProjectionBoundary(secondMethod.resultType.semantic)

      val noOp = right(ExistingClassUntypedRewrite(secondCapture, secondCapture.emptyPlan))
      assert(!noOp.changed)
      assert(noOp.tree.eq(first.tree))

      assertCode(
        secondCapture.emptyPlan.replaceParameterType(firstParameter.ref, canonicalOptionString),
        "SELECTION_FAILED"
      )
      assertCode(
        secondCapture.emptyPlan.replaceResultType(firstMethod.ref, canonicalOptionString),
        "SELECTION_FAILED"
      )

      val bodyOnly = right(ExistingClassUntypedRewrite(
        secondCapture,
        right(secondCapture.emptyPlan.replaceBody(secondMethod.ref, secondBody))
      ))
      assertCanonical(selectedParameter(bodyOnly.tree).tpt)
      assertCanonical(selectedMethod(bodyOnly.tree).tpt)

      val primitivePlan = right(for
        p1 <- secondCapture.emptyPlan.replaceParameterType(
          secondParameter.ref,
          TypeNormalForm.STypeIdent("String")
        )
        p2 <- p1.replaceResultType(secondMethod.ref, TypeNormalForm.STypeIdent("String"))
        p3 <- p2.replaceBody(secondMethod.ref, secondBody)
      yield p3)
      val primitive = right(ExistingClassUntypedRewrite(secondCapture, primitivePlan))
      assertDirectString(selectedParameter(primitive.tree).tpt)
      assertDirectString(selectedMethod(primitive.tree).tpt)

      val repeatedPlan = right(for
        p1 <- secondCapture.emptyPlan.replaceParameterType(secondParameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(secondMethod.ref, canonicalOptionString)
        p3 <- p2.replaceBody(secondMethod.ref, secondBody)
      yield p3)
      val repeated = right(ExistingClassUntypedRewrite(secondCapture, repeatedPlan))
      assertCanonical(selectedParameter(repeated.tree).tpt)
      assertCanonical(selectedMethod(repeated.tree).tpt)
      assert(repeated.directMemberIdentities(0).sameObjectAs(secondCapture.members(0).identity))
      assert(!repeated.directMemberIdentities(1).sameObjectAs(secondCapture.members(1).identity))
      assert(repeated.directMemberIdentities(2).sameObjectAs(secondCapture.members(2).identity))

  test("foreign package repeated pass keeps the full noncanonical semantic matrix fail closed"):
    withContext:
      val root = parseClass(fixtureSource)
      val firstCapture = right(ExistingClassUntypedRewrite.capture(root))
      val firstMethod = right(firstCapture.method(firstCapture.members(1).ref))
      val firstParameter = firstMethod.parameterClauses.head.parameters.head
      val firstPlan = right(for
        p1 <- firstCapture.emptyPlan.replaceParameterType(firstParameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(firstMethod.ref, canonicalOptionString)
      yield p2)
      val first = right(ExistingClassUntypedRewrite(firstCapture, firstPlan))
      val secondCapture = right(ExistingClassUntypedRewrite.capture(first.tree))
      val secondMethod = right(secondCapture.method(secondCapture.members(1).ref))
      val optionId = resolved(Vector(packageOwner("scala")), "Option")
      val stringId = resolved(Vector(packageOwner("java"), packageOwner("lang")), "String")
      val intId = resolved(Vector(packageOwner("scala")), "Int")
      val listId = resolved(
        Vector(packageOwner("scala"), packageOwner("collection"), packageOwner("immutable")),
        "List"
      )
      val aliasOwnerOption = resolved(Vector(packageOwner("aliases")), "Option")
      val shadowedString = resolved(Vector(packageOwner("aliases")), "String")
      val canonicalString = TypeNormalForm.STypeResolved(stringId)
      val cases = Vector(
        "unqualified Option and shadowable String" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("Option"),
          List(TypeNormalForm.STypeIdent("String"))
        ),
        "Option Int" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          List(TypeNormalForm.STypeResolved(intId))
        ),
        "imported List alias identity" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(listId),
          List(canonicalString)
        ),
        "resolved alias constructor" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(aliasOwnerOption),
          List(canonicalString)
        ),
        "bare Option" -> TypeNormalForm.STypeResolved(optionId),
        "zero arity Option" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          Nil
        ),
        "two argument Option" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          List(canonicalString, canonicalString)
        ),
        "shadowed String identity" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          List(TypeNormalForm.STypeResolved(shadowedString))
        )
      )

      cases.foreach { case (label, semantic) =>
        val plan = right(secondCapture.emptyPlan.replaceResultType(secondMethod.ref, semantic))
        assertCode(
          ExistingClassUntypedRewrite(secondCapture, plan),
          "UNSUPPORTED_STRUCTURE",
          label
        )
        assertCanonical(selectedParameter(first.tree).tpt)
        assertCanonical(selectedMethod(first.tree).tpt)
      }

  private val fixtureSource =
    """class RecaptureService:
      |  val before: Int = 1
      |  def echo(value: String): String = value
      |  val after: Int = 2
      |""".stripMargin

  private val canonicalOptionString: TypeNormalForm =
    val pkg = (name: String) => ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(ResolvedTypeNameId(Vector(pkg("scala")), "Option")),
      List(
        TypeNormalForm.STypeResolved(
          ResolvedTypeNameId(Vector(pkg("java"), pkg("lang")), "String")
        )
      )
    )

  private def resolved(
      owners: Vector[ResolvedTypeOwnerSegment],
      terminal: String
  ): ResolvedTypeNameId = ResolvedTypeNameId(owners, terminal)

  private def packageOwner(name: String): ResolvedTypeOwnerSegment =
    ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)

  private def selectedMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body(1).asInstanceOf[untpd.DefDef]

  private def selectedParameter(root: untpd.TypeDef)(using Context): untpd.ValDef =
    selectedMethod(root).paramss.head.head.asInstanceOf[untpd.ValDef]

  private def assertProjectionBoundary(result: Either[Failure, TypeNormalForm]): Unit =
    result match
      case Left(problem) =>
        assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
        assert(problem.detail.contains("U038 UNSUPPORTED_SELECTED_TYPE"), clues(problem))
      case Right(value) => fail(s"rooted applied Type unexpectedly projected as ${value.render}")

  private def assertCanonical(tree: untpd.Tree): Unit = tree match
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

  private def assertDirectString(tree: untpd.Tree): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, "String")
    case other => fail(s"expected direct String Type, found $other")

  private def assertCode[A](
      result: Either[Failure, A],
      expected: String,
      label: String = ""
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected, clues(label, problem))
    case Right(value) => fail(s"$label expected $expected, found success $value")

  private def right[A](result: Either[Failure, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("ExternalU074RootedOptionRecapture.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats.collect { case root: untpd.TypeDef => root } match
          case root :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
