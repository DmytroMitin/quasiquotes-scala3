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

final class ExistingClassUntypedRewriteOptionAppliedTypeFeasibilityTest extends munit.FunSuite:
  test("foreign package rewrites both method Types through the public facade with the exact resolved canonical pair"):
    withContext:
      val root = parseClass(
        """class OptionService:
          |  val before: Int = 1
          |  def echo(value: String): String = value
          |  val after: Int = 2
          |""".stripMargin
      )
      val captured = right(ExistingClassUntypedRewrite.capture(root))
      val method = right(captured.method(captured.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = right(method.parameterScope.reference(parameter.ref))
      val plan = right(for
        p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(method.ref, canonicalOptionString)
        p3 <- p2.replaceBody(method.ref, body)
      yield p3)

      val result = right(ExistingClassUntypedRewrite(captured, plan))
      assert(result.changed)
      assert(!result.tree.eq(root))
      assertEquals(result.directMemberIdentities.size, 3)
      assert(result.directMemberIdentities(0).sameObjectAs(captured.members(0).identity))
      assert(!result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assert(result.directMemberIdentities(2).sameObjectAs(captured.members(2).identity))

      val rewritten = result.tree.rhs.asInstanceOf[untpd.Template].body(1).asInstanceOf[untpd.DefDef]
      val rewrittenParameter = rewritten.paramss.head.head.asInstanceOf[untpd.ValDef]
      assertCanonicalRootedPair(rewrittenParameter.tpt)
      assertCanonicalRootedPair(rewritten.tpt)

  test("foreign package observes the exact current public Option[String] failure without a private bypass"):
    withContext:
      val root = parseClass(
        """class OptionService:
          |  val untouchedBefore: Int = 1
          |  def echo(value: String): String = value
          |  val untouchedAfter: Int = 2
          |""".stripMargin
      )
      val captured = right(ExistingClassUntypedRewrite.capture(root))
      val method = right(captured.method(captured.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = right(method.parameterScope.reference(parameter.ref))
      val optionString = TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("Option"),
        List(TypeNormalForm.STypeIdent("String"))
      )
      val plan = right(for
        p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, optionString)
        p2 <- p1.replaceResultType(method.ref, optionString)
        p3 <- p2.replaceBody(method.ref, body)
      yield p3)

      assertEquals(plan.editCount, 3)
      ExistingClassUntypedRewrite(captured, plan) match
        case Left(problem) =>
          assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
          assert(problem.detail.contains("U040 SEMANTIC_TYPE_FAMILY_UNSUPPORTED"), clues(problem))
          assert(problem.detail.contains("STypeApply"), clues(problem))
        case Right(value) => fail("Option[String] unexpectedly became a public capability: " + value)

  test("canonical resolved pair preserves no-op identity and both public intent orders are equivalent"):
    withContext:
      val firstRoot = parseClass(optionServiceSource)
      val firstCapture = right(ExistingClassUntypedRewrite.capture(firstRoot))
      val noOp = right(ExistingClassUntypedRewrite(firstCapture, firstCapture.emptyPlan))
      assert(!noOp.changed)
      assert(noOp.tree.eq(firstRoot))
      assert(firstCapture.members.indices.forall(index =>
        noOp.directMemberIdentities(index).sameObjectAs(firstCapture.members(index).identity)
      ))

      val parameterFirst = rewriteBoth(firstRoot, parameterFirst = true)
      val resultFirst = rewriteBoth(parseClass(optionServiceSource), parameterFirst = false)
      val firstMethod = selectedMethod(parameterFirst.tree)
      val secondMethod = selectedMethod(resultFirst.tree)
      assertCanonicalRootedPair(firstMethod.paramss.head.head.asInstanceOf[untpd.ValDef].tpt)
      assertCanonicalRootedPair(firstMethod.tpt)
      assertCanonicalRootedPair(secondMethod.paramss.head.head.asInstanceOf[untpd.ValDef].tpt)
      assertCanonicalRootedPair(secondMethod.tpt)
      assertEquals(firstMethod.toString, secondMethod.toString)
      assertEquals(parameterFirst.directMemberIdentities.size, resultFirst.directMemberIdentities.size)

  test("all noncanonical resolved and applied semantic values fail closed without changing the owner"):
    withContext:
      val root = parseClass(optionServiceSource)
      val captured = right(ExistingClassUntypedRewrite.capture(root))
      val method = right(captured.method(captured.members(1).ref))
      val oldMethod = selectedMethod(root)
      val optionId = resolved(Vector(packageOwner("scala")), "Option")
      val stringId = resolved(Vector(packageOwner("java"), packageOwner("lang")), "String")
      val intId = resolved(Vector(packageOwner("scala")), "Int")
      val listId = resolved(
        Vector(packageOwner("scala"), packageOwner("collection"), packageOwner("immutable")),
        "List"
      )
      val eitherId = resolved(Vector(packageOwner("scala"), packageOwner("util")), "Either")
      val wrongOwnerOption = resolved(Vector(packageOwner("example")), "Option")
      val wrongStringOwner = resolved(Vector(packageOwner("scala")), "String")
      val typeOwnedOption = resolved(
        Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Type, "scala")),
        "Option"
      )
      val canonicalString = TypeNormalForm.STypeResolved(stringId)
      val cases = Vector(
        "name only" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("Option"),
          List(TypeNormalForm.STypeIdent("String"))
        ),
        "option int" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          List(TypeNormalForm.STypeResolved(intId))
        ),
        "list string" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(listId),
          List(canonicalString)
        ),
        "either" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(eitherId),
          List(canonicalString, canonicalString)
        ),
        "nested option" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          List(canonicalOptionString)
        ),
        "bare option" -> TypeNormalForm.STypeResolved(optionId),
        "zero arity" -> TypeNormalForm.STypeApply(TypeNormalForm.STypeResolved(optionId), Nil),
        "two arguments" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          List(canonicalString, canonicalString)
        ),
        "foreign option owner" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(wrongOwnerOption),
          List(canonicalString)
        ),
        "type owned option" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(typeOwnedOption),
          List(canonicalString)
        ),
        "wrong string owner" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          List(TypeNormalForm.STypeResolved(wrongStringOwner))
        ),
        "null option identity" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(null),
          List(canonicalString)
        ),
        "null string identity" -> TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(optionId),
          List(TypeNormalForm.STypeResolved(null))
        )
      )

      cases.foreach { case (label, semantic) =>
        val plan = right(captured.emptyPlan.replaceResultType(method.ref, semantic))
        ExistingClassUntypedRewrite(captured, plan) match
          case Left(problem) => assertEquals(problem.code, "UNSUPPORTED_STRUCTURE", clues(label, problem))
          case Right(result) => fail(s"$label unexpectedly rewrote the owner: $result")
        assert(selectedMethod(root).eq(oldMethod))
      }

  test("canonical applied replacement stays fail closed for unsupported method shapes and atomic mixed failure"):
    withContext:
      val unsupportedSources = Vector(
        "by-name" -> "class C { def echo(value: => String): String = value }",
        "repeated" -> "class C { def echo(value: String*): String = value.head }",
        "contextual" -> "class C { def echo(using value: String): String = value }",
        "zero parameter" -> "class C { def echo(): String = throw new RuntimeException() }",
        "generic dependent" -> "class C { def echo[A](value: A): A = value }"
      )
      unsupportedSources.foreach { case (label, source) =>
        val root = parseClass(source)
        val captured = right(ExistingClassUntypedRewrite.capture(root))
        val methodIndex = captured.members.indexWhere(_.kind == MemberKind.Method)
        val method = right(captured.method(captured.members(methodIndex).ref))
        val plan = right(captured.emptyPlan.replaceResultType(method.ref, canonicalOptionString))
        ExistingClassUntypedRewrite(captured, plan) match
          case Left(problem) => assertEquals(problem.code, "UNSUPPORTED_STRUCTURE", clues(label, problem))
          case Right(result) => fail(s"$label unexpectedly rewrote the owner: $result")
      }

      val root = parseClass(optionServiceSource)
      val captured = right(ExistingClassUntypedRewrite.capture(root))
      val method = right(captured.method(captured.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      val oldMethod = selectedMethod(root)
      val invalid = TypeNormalForm.STypeApply(
        TypeNormalForm.STypeResolved(resolved(Vector(packageOwner("scala")), "Option")),
        List(TypeNormalForm.STypeIdent("Int"))
      )
      val plan = right(for
        p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, TypeNormalForm.STypeIdent("Int"))
        p2 <- p1.replaceResultType(method.ref, invalid)
      yield p2)
      assert(ExistingClassUntypedRewrite(captured, plan).isLeft)
      assert(selectedMethod(root).eq(oldMethod))

  private val canonicalOptionString: TypeNormalForm =
    val pkg = (name: String) =>
      ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(
        ResolvedTypeNameId(Vector(pkg("scala")), "Option")
      ),
      List(
        TypeNormalForm.STypeResolved(
          ResolvedTypeNameId(Vector(pkg("java"), pkg("lang")), "String")
        )
      )
    )

  private val optionServiceSource =
    """class OptionService:
      |  val before: Int = 1
      |  def echo(value: String): String = value
      |  val after: Int = 2
      |""".stripMargin

  private def packageOwner(name: String): ResolvedTypeOwnerSegment =
    ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)

  private def resolved(
      owners: Vector[ResolvedTypeOwnerSegment],
      terminal: String
  ): ResolvedTypeNameId = ResolvedTypeNameId(owners, terminal)

  private def rewriteBoth(root: untpd.TypeDef, parameterFirst: Boolean)(using Context): Result =
    val captured = right(ExistingClassUntypedRewrite.capture(root))
    val method = right(captured.method(captured.members(1).ref))
    val parameter = method.parameterClauses.head.parameters.head
    val plan =
      if parameterFirst then
        right(for
          p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, canonicalOptionString)
          p2 <- p1.replaceResultType(method.ref, canonicalOptionString)
        yield p2)
      else
        right(for
          p1 <- captured.emptyPlan.replaceResultType(method.ref, canonicalOptionString)
          p2 <- p1.replaceParameterType(parameter.ref, canonicalOptionString)
        yield p2)
    right(ExistingClassUntypedRewrite(captured, plan))

  private def selectedMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body(1).asInstanceOf[untpd.DefDef]

  private def assertCanonicalRootedPair(tree: untpd.Tree): Unit =
    tree match
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
      case other =>
        fail(s"expected canonical rooted Option[String], found ${other.getClass.getSimpleName}: $other")

  private def right[A](result: Either[Failure, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("ExternalU071OptionAppliedType.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
