package quasiquotes.types

import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

class RuntimeTypeApplicationTest extends munit.FunSuite:
  test("runtime applications preserve every caller object and agree with source oracles"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      def run(using q: Quotes): Unit =
        import q.reflect.*
        import QuasiTypequotes.*
        def constructor(t: TypeRepr): TypeRepr = t match
          case AppliedType(c, _) => c
        def checked(c: TypeRepr, args: Seq[TypeRepr], oracle: TypeRepr): TypeRepr =
          val result: TypeRepr = tqr"$c[..$args]"
          assert(result =:= oracle)
          result match
            case AppliedType(found, elements) =>
              assert(found.asInstanceOf[AnyRef] eq c.asInstanceOf[AnyRef])
              assertEquals(elements.size, args.size)
              elements.zip(args).foreach((a, b) => assert(a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]))
            case _ => fail("Expected AppliedType")
          result
        val list = constructor(TypeRepr.of[List[Int]])
        val either = constructor(TypeRepr.of[Either[Int, String]])
        val option = constructor(TypeRepr.of[Option[String]])
        val int = TypeRepr.of[Int]
        val string = TypeRepr.of[String]
        val constructors = Vector(list, either)
        val arguments = Vector(Vector(int), Vector(int, string))
        val oracles = Vector(TypeRepr.of[List[Int]], TypeRepr.of[Either[Int, String]])
        constructors.zip(arguments).zip(oracles).foreach { case ((c, args), oracle) => checked(c, args, oracle) }
        val left = checked(list, Seq(int), TypeRepr.of[List[Int]])
        val right = checked(option, Seq(string), TypeRepr.of[Option[String]])
        checked(either, Seq(left, right), TypeRepr.of[Either[List[Int], Option[String]]])
        checked(constructor(TypeRepr.of[Q005HigherKindedBox[List]]), Seq(list), TypeRepr.of[Q005HigherKindedBox[List]])

      run(using summon[Quotes])

  test("external direct and umbrella imports expose the dependent result type"):
    assert(external.consumer.RuntimeTypeApplicationConsumers.direct)
    assert(external.consumer.RuntimeTypeApplicationConsumers.umbrella)

  test("existing scalar overloads remain source-unambiguous"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      def run(using q: Quotes): Unit =
        import q.reflect.*
        import QuasiTypequotes.*
        val int = TypeRepr.of[Int]
        val string = TypeRepr.of[String]
        assert(tqr"List[$int]" =:= TypeRepr.of[List[Int]])
        assert(tqr"Either[$int, $string]" =:= TypeRepr.of[Either[Int, String]])
        assert(tqr"Either[List[$int], Option[$string]]" =:= TypeRepr.of[Either[List[Int], Option[String]]])
      run(using summon[Quotes])


  test("the production postcondition rejects mismatched reflection results"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      def run(using q: Quotes): Unit =
        import q.reflect.*
        def constructor(t: TypeRepr): TypeRepr = t match
          case AppliedType(c, _) => c
        val list = constructor(TypeRepr.of[List[Int]])
        val option = constructor(TypeRepr.of[Option[Int]])
        val either = constructor(TypeRepr.of[Either[Int, String]])
        val int = TypeRepr.of[Int]
        val string = TypeRepr.of[String]
        val malformed = List(
          (list, List(int), AppliedType(option, List(int))),
          (list, List(int), AppliedType(list, List(string))),
          (list, List(int), AppliedType(list, Nil)),
          (either, List(int, string), AppliedType(either, List(string, int))),
          (list, List(int), int)
        )
        malformed.foreach { (c, args, result) =>
          val error = ReflectedTypeApplication.verifyResult(using q)(c, args, result).left.toOption
          assert(error.exists(_.startsWith("TYPE_REFLECTION_APPLICATION_INVARIANT:")))
        }
      run(using summon[Quotes])
