package external.consumer

import java.lang.invoke.{MethodHandles, MethodType}

import quasiquotes.matching.{TermPattern, TermPatternProductExtractor}

final class Q059ScalametaSelectedMemberExternalConsumerTest extends munit.FunSuite:
  test("external direct and umbrella imports expand selected-name patterns"):
    assertEquals(
      Q059ScalametaSelectedMemberExternalConsumer.directObservations,
      List("ordinary", "1")
    )
    assertEquals(
      Q059ScalametaSelectedMemberExternalConsumer.umbrellaObservations,
      List("field", "nullary")
    )

  test("Q058 precompiled selected-name factory descriptors remain linkable"):
    val factoryClass = Class.forName(
      "quasiquotes.matching.TermPatternProductExtractorFactory$"
    )
    val descriptor = MethodType.methodType(
      classOf[TermPatternProductExtractor[?, ?]],
      classOf[scala.quoted.Quotes],
      classOf[TermPattern],
      classOf[scala.collection.immutable.Vector[?]],
      classOf[String]
    )
    List("direct", "nullary", "unary").foreach { methodName =>
      MethodHandles.publicLookup().findVirtual(factoryClass, methodName, descriptor)
    }
