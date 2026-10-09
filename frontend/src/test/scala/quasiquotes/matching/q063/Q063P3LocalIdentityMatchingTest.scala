package quasiquotes.matching.q063

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.matching.{
  BlockPatternStatement,
  QuasiPattern,
  RankedPatternSource,
  TermPattern,
  TermPatternExtractor
}
import quasiquotes.types.TypeNormalForm
import Q063P3LocalIdentityMatchingMacros.*

private object Q063P3LocalIdentityMatchingFixtures:
  private val ambient = 41

  val exact = evidence {
    def id(value: Int): Int = value
    id(41)
  }

  val alphaRenamedAtBinder = evidence {
    def different(`x@y`: Int): Int = `x@y`
    different(43)
  }

  val alphaRenamed = evidence {
    def different(x: Int): Int = x
    different(42)
  }

  val stringIdentity = captureString {
    def different(x: String): String = x
    different("value")
  }

  val booleanIdentity = captureBoolean {
    def different(x: Boolean): Boolean = x
    different(true)
  }

  val ambientArgument = captureInt {
    def different(x: Int): Int = x
    different(ambient)
  }

  val repeatedAmbient = captureRepeated {
    def different(x: Int): Int = x
    different((ambient, ambient)._1)
  }

  val negativeResults = List(
    matchesInt {
      def different(x: String): String = x
      different("value")
    },
    matchesInt[AnyVal] {
      def different(x: Int): AnyVal = x
      different(41)
    },
    matchesInt {
      def different(x: Int): Int = 1
      different(41)
    },
    matchesInt {
      def different(x: Int): Int =
        if x == 0 then x else different(x - 1)
      different(1)
    },
    matchesInt {
      def other(x: Int): Int = x
      identity(41)
    },
    matchesInt {
      def first(x: Int): Int = x
      def second(x: Int): Int = x
      first(41)
    },
    matchesInt {
      val seed: Int = 41
      def different(x: Int): Int = x
      different(seed)
    },
    matchesInt {
      def different(x: Int): Int =
        def nested(y: Int): Int = y
        nested(x)
      different(41)
    },
    matchesInt {
      def different(x: Int): Int = x
      different
    },
    matchesInt {
      def different()(x: Int): Int = x
      different()(41)
    },
    matchesInt {
      def different(x: Int)(y: Int): Int = x
      different(41)(1)
    },
    matchesInt {
      def different(x: Int, y: Int): Int = x
      different(41, 1)
    },
    matchesInt {
      def different[A](x: Int): Int = x
      different[Int](41)
    },
    matchesInt {
      inline def different(x: Int): Int = x
      different(41)
    },
    matchesInt {
      infix def different(x: Int): Int = x
      different(41)
    },
    matchesInt {
      implicit def different(x: Int): Int = x
      different(41)
    },
    matchesInt {
      @deprecated("unsupported", "Q063")
      def different(x: Int): Int = x
      different(41)
    },
    matchesInt {
      def different(@deprecated("unsupported", "Q063") x: Int): Int = x
      different(41)
    },
    matchesInt {
      def different(x: Int = 1): Int = x
      different()
    },
    matchesInt {
      def different(x: => Int): Int = x
      different(41)
    },
    matchesInt {
      def different(x: Int*): Int = x.head
      different(41)
    },
    matchesInt {
      def different(x: Int) = x
      different(41)
    },
    matchesInt {
      def different(x: Double): Double = x
      different(41.0)
    },
    matchesInt(41),
    matchesInt(null)
  )
class Q063P3LocalIdentityMatchingTest extends munit.FunSuite:
  import Q063P3LocalIdentityMatchingFixtures.*

  test("standard qq matches exact and alpha-renamed Int local identity methods"):
    List(exact, alphaRenamed, alphaRenamedAtBinder).foreach { result =>
      assert(result.value > 0, result)
      assert(result.exactArgumentIdentity, result)
      assert(result.distinctBinderSymbols, result)
      assert(result.bodyUsesParameterSymbol, result)
      assert(result.resultUsesMethodSymbol, result)
    }
    assertEquals(exact.value, 41)
    assertEquals(alphaRenamed.value, 42)

    assertEquals(alphaRenamedAtBinder.value, 43)
  test("fixed semantic String and Boolean Types match"):
    assertEquals(stringIdentity, "value")
    assertEquals(booleanIdentity, true)

  test("ambient Symbols stay outside local scopes and repeated holes retain equality"):
    assertEquals(ambientArgument, 41)
    assertEquals(repeatedAmbient, 41)

  test("wrong Types, bodies, calls, topology, clauses, modifiers, and targets reject atomically"):
    negativeResults.zipWithIndex.foreach { case (result, index) =>
      assert(!result, s"negative target $index unexpectedly matched")
    }

  test("P3 pattern admission requires one semantic scalar hole, allowing repetitions"):
    val compiled = QuasiPattern
      .term("{ def id(value: Int): Int = value; id($argument) }")
      .toOption
      .get
      .pattern
    compiled match
      case TermPattern.Block(
            List(
              BlockPatternStatement.LocalDef(
                methodBinderId,
                "id",
                parameterBinderId,
                "value",
                TypeNormalForm.STypeIdent("Int"),
                TypeNormalForm.STypeIdent("Int"),
                TermPattern.BoundReference(bodyBinderId, "value")
              )
            ),
            TermPattern.Apply(
              TermPattern.BoundReference(resultBinderId, "id"),
              List(TermPattern.Hole("argument"))
            )
          ) =>
        assertNotEquals(methodBinderId, parameterBinderId)
        assertEquals(bodyBinderId, parameterBinderId)
        assertEquals(resultBinderId, methodBinderId)
      case other => fail(s"unexpected Q063 private IR: ${other.render}")
    assertEquals(
      compiled.render,
      "Block([LocalDef(id(value: STypeIdent(Int)): STypeIdent(Int) = BoundRef(value))], " +
        "Apply(BoundRef(id), [Hole($argument)]))"
    )

    assert(
      QuasiPattern
        .term("{ def id(value: Int): Int = value; id(41) }")
        .isLeft
    )
    assert(
      QuasiPattern
        .term(
          "{ def id(value: Int): Int = value; id(($first, $second)._1) }"
        )
        .isLeft
    )
    assert(
      QuasiPattern
        .term("foo({ def id(value: Int): Int = value; id($argument) })")
        .isLeft
    )
    assert(
      QuasiPattern
        .term(
          "({ def id(value: Int): Int = value; id($argument) }, " +
            "{ def other(x: Int): Int = x; other($argument) })"
        )
        .isLeft
    )


  test("dynamic StringContext retains the truthful scalar fallback type"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.{QuasiPattern, TermPatternExtractor}
        def probe(using q: Quotes)(context: StringContext) =
          val extractor: TermPatternExtractor[q.reflect.Term] =
            QuasiPattern.qq(context)(using q)
          extractor
      }"""
    )
    assertEquals(errors, Nil)
  test("a parameter Symbol cannot escape its method-body scope"):
    assertEquals(escapedParameterMatches, false)


  test("P3 stays scalar-only and rejects ranked sequence capture"):
    val compiled = RankedPatternSource.compile(
      List("{ def id(value: Int): Int = value; id(..", ") }"),
      sequenceIndex = 0
    )
    assertEquals(
      compiled,
      Left("rank-2 sequence-Term capture is not supported in P3 local identity method patterns")
    )
