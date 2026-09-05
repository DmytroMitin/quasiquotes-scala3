package quasiquotes.definitions

class DefinitionNameTest extends munit.FunSuite:
  private def acceptedInternal(result: Either[DefinitionError, DefinitionName]): DefinitionName =
    result.fold(error => fail(error.message), identity)

  private def acceptedPublic(
      result: Either[DefinitionSemanticError, DefinitionName]
  ): DefinitionName =
    result.fold(error => fail(error.message), identity)

  test("plain names retain exact decoded source and structural rendering") {
    val expected = Vector("answer", "answer1", "_answer")

    expected.foreach { source =>
      val name = acceptedInternal(DefinitionName.plain(source))
      assertEquals(name.decoded, source)
      assertEquals(name.source, source)
      assertEquals(name.spelling, DefinitionNameSpelling.Plain)
      assertEquals(name.render, s"PlainName($source)")
      assertEquals(name, acceptedPublic(DefinitionName.fromSource(source)))
    }
  }

  test("backticked keyword-style names retain decoded and source spelling") {
    val expected = Vector("`type`" -> "type", "`match`" -> "match", "`extension`" -> "extension")

    expected.foreach { case (source, decoded) =>
      val name = acceptedInternal(DefinitionName.backticked(source))
      assertEquals(name.decoded, decoded)
      assertEquals(name.source, source)
      assertEquals(name.spelling, DefinitionNameSpelling.BacktickedKeyword)
      assertEquals(name.render, s"BacktickedKeywordName($source)")
      assertEquals(name, acceptedPublic(DefinitionName.fromSource(source)))
    }
  }

  test("plain policy rejects empty keyword digit whitespace qualified symbolic dollar backtick and Unicode names") {
    val rejected = Vector(
      "",
      "_",
      "type",
      "match",
      "extension",
      "1answer",
      "answer value",
      "owner.answer",
      "+",
      "answer$",
      "`answer`",
      "café",
      "Δ"
    )

    rejected.foreach { source =>
      val error = DefinitionName.plain(source).left.toOption.getOrElse(fail(s"accepted $source"))
      assertEquals(error, DefinitionError.InvalidPlainName(source))
      assert(error.message.startsWith("Invalid plain definition name"))
    }
  }

  test("backticked policy rejects non-keywords malformed pairs embedded backticks and line breaks") {
    val rejected = Vector(
      "",
      "type",
      "``",
      "`answer`",
      "``type``",
      "`ty`pe`",
      "`type",
      "type`",
      "`type`extra",
      "`type\n`",
      "`type\r`"
    )

    rejected.foreach { source =>
      val error = DefinitionName.backticked(source).left.toOption.getOrElse(fail(s"accepted $source"))
      assertEquals(error, DefinitionError.InvalidBacktickedName(source))
      assert(error.message.startsWith("Invalid backticked definition name"))
    }
  }

  test("invalid-name messages escape line breaks deterministically") {
    assertEquals(
      DefinitionName.backticked("`type\n`").left.toOption.get.message,
      "Invalid backticked definition name '`type\\n`': expected exactly one backtick pair around a reserved Scala 3 keyword."
    )
  }
