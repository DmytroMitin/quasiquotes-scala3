package quasiquotes.neutral

import scala.meta.*
import scala.meta.dialects.Scala3
import scala.annotation.nowarn

@nowarn("cat=deprecation")
final class ScalametaTermApplyAstCharacterizationTest extends munit.FunSuite:
  test("canonical identifier, selection, and ordinary Apply syntax has the expected typed topology"):
    val identifier: Term = q"f"
    val selection: Term = q"obj.inner.f"
    val applications: List[Term] =
      List(q"f()", q"f(1)", q"f(1, 2)", q"obj.f(1 + 2)")

    assert(identifier.isInstanceOf[Term.Name])

    selection match
      case outer: Term.Select =>
        assertEquals(outer.name.value, "f")
        outer.qual match
          case inner: Term.Select =>
            assertEquals(inner.name.value, "inner")
            assertEquals(inner.qual.asInstanceOf[Term.Name].value, "obj")
          case other => fail(s"expected nested Term.Select, found ${other.productPrefix}")
      case other => fail(s"expected Term.Select, found ${other.productPrefix}")

    val expectedArgumentCounts = List(0, 1, 2, 1)
    applications.zip(expectedArgumentCounts).foreach { (source, expectedCount) =>
      source match
        case application: Term.Apply =>
          assertEquals(application.argClause.mod, None)
          assertEquals(application.argClause.values.size, expectedCount)
        case other => fail(s"expected Term.Apply, found ${other.productPrefix}")
    }

    val selectedCall = applications.last.asInstanceOf[Term.Apply]
    assert(selectedCall.fun.isInstanceOf[Term.Select])
    assert(selectedCall.argClause.values.head.isInstanceOf[Term.ApplyInfix])

  test("neighboring argument and application-clause syntax is structurally distinguishable"):
    val multipleLists = q"f(1)(2)".asInstanceOf[Term.Apply]
    val typeApplication = q"f[Int](1)".asInstanceOf[Term.Apply]
    val contextual = q"f(using 1)".asInstanceOf[Term.Apply]
    val named = q"f(value = 1)".asInstanceOf[Term.Apply]
    val repeated = q"f(values*)".asInstanceOf[Term.Apply]

    assert(multipleLists.fun.isInstanceOf[Term.Apply])
    assert(typeApplication.fun.isInstanceOf[Term.ApplyType])
    assert(contextual.argClause.mod.exists(_.isInstanceOf[Mod.Using]))
    assert(named.argClause.values.head.isInstanceOf[Term.Assign])
    assert(repeated.argClause.values.head.isInstanceOf[Term.Repeated])

  test("representative neighboring Term families remain distinct from the admitted nodes"):
    val neighboring = List[Term](
      q"x => x",
      Term.ApplyUnary(Term.Name("-"), Lit.Int(1)),
      q"(1, 2)",
      q"{ val x = 1; x }",
      q"if true then 1 else 2",
      q"(1: Int)",
      Input.String("s\"value=$f\"").parse[Term].get,
      q"new java.lang.StringBuilder(16)"
    )

    assertEquals(
      neighboring.map(_.productPrefix),
      List(
        "Term.Function",
        "Term.ApplyUnary",
        "Term.Tuple",
        "Term.Block",
        "Term.If",
        "Term.Ascribe",
        "Term.Interpolate",
        "Term.New"
      )
    )

  test("signed literals, structural unary terms, tuples, and if forms have distinguishable topology"):
    val negativeLiteral = q"-1"
    val positiveLiteral = q"+1"
    val structuralUnary = q"!flag"
    val tuple = q"(1, true)"
    val explicitElse = Input.String("if true then 1 else 2").parse[Term].get
    val noElse = Input.String("if true then 1").parse[Term].get
    val explicitUnitElse = Input.String("if true then 1 else ()").parse[Term].get

    assertEquals(negativeLiteral.asInstanceOf[Lit.Int].value, -1)
    assertEquals(positiveLiteral.asInstanceOf[Lit.Int].value, 1)
    assert(structuralUnary.isInstanceOf[Term.ApplyUnary])
    assertEquals(tuple.asInstanceOf[Term.Tuple].args.size, 2)
    intercept[Exception](
      Term.ApplyUnary(Term.Name("custom"), Term.Name("value"))
    )

    val explicitConditional = explicitElse.asInstanceOf[Term.If]
    assert(!explicitConditional.elsep.isInstanceOf[Lit.Unit])

    val noElseConditional = noElse.asInstanceOf[Term.If]
    assert(noElseConditional.elsep.isInstanceOf[Lit.Unit])
    assertEquals(noElseConditional.elsep.pos.start, noElseConditional.elsep.pos.end)

    val explicitUnitConditional = explicitUnitElse.asInstanceOf[Term.If]
    assert(explicitUnitConditional.elsep.isInstanceOf[Lit.Unit])
    assert(explicitUnitConditional.elsep.pos.start < explicitUnitConditional.elsep.pos.end)
