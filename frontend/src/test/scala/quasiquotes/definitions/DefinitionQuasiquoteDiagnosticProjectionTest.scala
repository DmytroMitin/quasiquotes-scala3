package quasiquotes.definitions

import quasiquotes.source.*
import quasiquotes.types.TypeNormalForm

class DefinitionQuasiquoteDiagnosticProjectionTest extends munit.FunSuite:
  import DefinitionQuasiquotes.*
  import DefinitionQuasiquoteError.*
  import DefinitionQuasiquoteTestFixtures.*

  test("wrong-role descriptors fail at their exact interpolation argument without internal names") {
    val cases = Vector[
      () => Either[LocatedDiagnostic[DefinitionQuasiquoteError], DefinitionQuasiquoteResult]
    ](
      () => dqr"def answer: ${bodyType("Int")} = 1",
      () => dqr"def answer: ${bodyTerm("value")} = 1",
      () => dqr"def answer: Int = ${definitionType("Int")}",
      () => dqr"def answer: Int = (1: ${definitionType("Int")})",
      () => dqr"def answer: Int = ${bodyType("Int")}",
      () => dqr"def ${bodyTerm("name")}: Int = 1"
    )

    cases.foreach { run =>
      val failure = run().left.toOption.get
      val projected = failure.diagnostic.asInstanceOf[FrontendFailure]
      assertEquals(projected.argumentIndex, Some(0))
      assertEquals(
        failure.location.toVector.flatMap(_.origins).collect {
          case origin: SourceOrigin.InterpolationArgument => origin.argumentIndex
        }.distinct,
        Vector(0)
      )
      assertNoInternalNames(failure.diagnostic.message)
    }
  }

  test("invalid completed type is attributed to its unique argument and preserves structured cause") {
    val invalid = DefinitionArguments.definitionType(
      TypeNormalForm.STypeIdent("NotInTheAdmittedTypeSubset")
    )
    val failure =
      dqr"def answer: $invalid = 1".left.toOption.get
    val projected = failure.diagnostic.asInstanceOf[CompletionFailure]

    assertEquals(projected.argumentIndex, Some(0))
    assertEquals(projected.role, Some("definition type"))
    assert(
      projected.underlying
        .isInstanceOf[DefinitionConstructionError.InvalidTypeBinding]
    )
    assertNoInternalNames(projected.message)
    assertEquals(
      failure.location.toVector.flatMap(_.origins).collect {
        case origin: SourceOrigin.InterpolationArgument => origin.argumentIndex
      },
      Vector(0)
    )
  }

  test("malformed literal syntax remains a literal or whole-definition diagnostic") {
    val failure = dqr"def answer: Int = (".left.toOption.get
    assert(failure.diagnostic.isInstanceOf[FrontendFailure])
    assertEquals(
      failure.location.toVector.flatMap(_.origins).collect {
        case _: SourceOrigin.InterpolationArgument => 1
      },
      Vector.empty
    )
    assertNoInternalNames(failure.diagnostic.message)
  }

  test("reused descriptor failures still resolve by occurrence identity rather than payload identity") {
    val invalid = DefinitionArguments.bodyType(
      TypeNormalForm.STypeIdent("NotInTheAdmittedTypeSubset")
    )
    val failure =
      dqr"def answer: Int = (1: $invalid)".left.toOption.get
    val projected = failure.diagnostic.asInstanceOf[CompletionFailure]
    assertEquals(projected.argumentIndex, Some(0))
    assertNoInternalNames(projected.message)
  }

  test("unexplained active dollar syntax and rejected definition forms remain deterministic") {
    val unexplained =
      StringContext("def answer: Int = $unexplained").dqr().left.toOption.get
    assert(unexplained.diagnostic.isInstanceOf[FrontendFailure])
    assertNoInternalNames(unexplained.diagnostic.message)

    val rejected = Vector(
      "def answer(value: Int): Int = value",
      "def answer[A]: Int = 1",
      "def answer = 1",
      "var answer: Int = 1",
      "lazy val answer: Int = 1",
      "type Answer = Int",
      "class Answer",
      "object Answer",
      "trait Answer",
      "enum Answer",
      "def first: Int = 1; def second: Int = 2"
    )
    rejected.foreach { source =>
      val failure = StringContext(source).dqr().left.toOption.get
      assert(failure.diagnostic.isInstanceOf[FrontendFailure], source)
      assertNoInternalNames(failure.diagnostic.message)
    }
  }

  private def assertNoInternalNames(message: String): Unit =
    assert(!message.contains("definitionArgument"), message)
    assert(!message.contains("__qq_dt_"), message)
