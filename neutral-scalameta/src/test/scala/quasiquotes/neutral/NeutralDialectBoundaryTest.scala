package quasiquotes.neutral

import scala.meta.*

class NeutralDialectBoundaryTest extends munit.FunSuite:
  test("Scala38 parses the bounded modern syntax compatibility sample") {
    val scala38: Dialect = dialects.Scala38
    val source = scala38(
      """enum Color derives CanEqual:
        |  case Red, Green
        |
        |extension (value: String)
        |  def nonEmptySize(using DummyImplicit): Int = value.size
        |
        |opaque type UserId = Long
        |given Ordering[UserId] = Ordering.Long
        |""".stripMargin
    ).parse[Source].get

    assertEquals(source.stats.size, 4)
    assert(source.syntax.contains("enum Color derives CanEqual"))
    assert(source.syntax.contains("extension (value: String)"))
    assert(source.syntax.contains("using DummyImplicit"))
    assert(source.syntax.contains("opaque type UserId"))
    assert(source.syntax.contains("given Ordering[UserId]"))
  }

  test("Scala3Future parses a tracked parameter without changing quasiquote dialect scope") {
    val futureDialect: Dialect = dialects.Scala3Future
    val source =
      futureDialect("class Vec(tracked val size: Int)").parse[Source].get

    assert(source.syntax.contains("tracked val size"))
  }
