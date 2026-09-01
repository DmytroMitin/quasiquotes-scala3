package quasiquotes.q002

class Q002PublicApiCompatibilityTest extends munit.FunSuite:
  test("transparent qq retains the legacy scalar JVM entry descriptor"):
    val method = Class
      .forName("quasiquotes.matching.QuasiPattern$")
      .getMethods
      .find(method =>
        method.getName == "qq" &&
          method.getParameterTypes.toList.map(_.getName) ==
            List("scala.StringContext", "scala.quoted.Quotes")
      )

    assert(method.nonEmpty)
    assertEquals(
      method.get.getReturnType.getName,
      "quasiquotes.matching.TermPatternExtractor"
    )
