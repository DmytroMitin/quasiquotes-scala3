package quasiquotes.types

import scala.quoted.staging.{Compiler, withQuotes}
import scala.compiletime.testing.typeCheckErrors
import quasiquotes.types.phase119.*

class GlobalSelectedTypeFrontendTest extends munit.FunSuite:
  private inline def typeErrorMessages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)
  test("explicit environment constructs canonical global terminals and exact fixed constructors"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val top = TypeRepr.of[TopLevel]
      val environment = GlobalSelectedTypeEnvironment
        .fromWitnesses(using q)(
          top,
          TypeRepr.of[OwnerOne.Nested],
          TypeRepr.of[List[Int]],
          TypeRepr.of[Option[String]],
          TypeRepr.of[Either[Int, String]]
        )
        .toOption
        .get

      val terminal = GlobalSelectedTypeFrontend.construct(using q)(
        "quasiquotes.types.phase119.TopLevel",
        environment
      ).toOption.get
      val list = GlobalSelectedTypeFrontend.construct(using q)(
        "scala.collection.immutable.List[Int]",
        environment
      ).toOption.get
      val option = GlobalSelectedTypeFrontend.construct(using q)(
        "scala.Option[String]",
        environment
      ).toOption.get
      val either = GlobalSelectedTypeFrontend.construct(using q)(
        "scala.util.Either[Int, String]",
        environment
      ).toOption.get
      (
        terminal.asInstanceOf[AnyRef].eq(top.asInstanceOf[AnyRef]),
        TargetTypeReprInspector.inspectResolved(using q)(list, environment).map(_.render),
        TargetTypeReprInspector.inspectResolved(using q)(option, environment).map(_.render),
        TargetTypeReprInspector.inspectResolved(using q)(either, environment).map(_.render)
      )

    assert(evidence._1)
    assert(evidence._2.exists(_.contains("Package(scala)/Package(collection)/Package(immutable)::List")), evidence._2)
    assert(evidence._3.exists(_.contains("Package(scala)::Option")), evidence._3)
    assert(evidence._4.exists(_.contains("Package(scala)/Package(util)::Either")), evidence._4)

  test("literal, constructor, ordinary, and repeated captures preserve bounded structural semantics"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val environment = GlobalSelectedTypeEnvironment
        .fromWitnesses(using q)(
          TypeRepr.of[TopLevel],
          TypeRepr.of[OwnerOne.Same],
          TypeRepr.of[OwnerTwo.Same],
          TypeRepr.of[List[Int]],
          TypeRepr.of[Either[Int, String]]
        )
        .toOption
        .get

      val literal = GlobalSelectedTypeFrontend.compilePattern(using q)(
        "quasiquotes.types.phase119.OwnerOne.Same",
        environment
      ).toOption.get
      val literalSuccess = GlobalSelectedTypeFrontend.matchPattern(using q)(
        literal,
        TypeRepr.of[OwnerOne.Same],
        environment
      )
      val differentOwner = GlobalSelectedTypeFrontend.matchPattern(using q)(
        literal,
        TypeRepr.of[OwnerTwo.Same],
        environment
      )

      val constructor = GlobalSelectedTypeFrontend.compilePattern(using q)(
        "scala.collection.immutable.List[$item]",
        environment
      ).toOption.get
      val constructorTarget = TypeRepr.of[List[Int]]
      val targetItem = constructorTarget match
        case AppliedType(_, item :: Nil) => item
      val constructorCapture = GlobalSelectedTypeFrontend
        .matchPattern(using q)(constructor, constructorTarget, environment)
        .toOption.flatten.get("item")

      val tupleTarget = TypeRepr.of[List[(Int, String)]]
      val targetTuple = tupleTarget match
        case AppliedType(_, item :: Nil) => item
      val tupleCapture = GlobalSelectedTypeFrontend
        .matchPattern(using q)(constructor, tupleTarget, environment)
        .toOption.flatten.get("item")

      val repeated = GlobalSelectedTypeFrontend.compilePattern(using q)(
        "Either[$same, $same]",
        environment
      ).toOption.get
      val repeatedSuccess = GlobalSelectedTypeFrontend.matchPattern(using q)(
        repeated,
        TypeRepr.of[Either[TopLevel, TopLevel]],
        environment
      )
      val repeatedFailure = GlobalSelectedTypeFrontend.matchPattern(using q)(
        repeated,
        TypeRepr.of[Either[TopLevel, OwnerOne.Same]],
        environment
      )
      (
        literalSuccess.map(_.nonEmpty),
        differentOwner.map(_.isEmpty),
        constructorCapture.asInstanceOf[AnyRef].eq(targetItem.asInstanceOf[AnyRef]),
        tupleCapture.asInstanceOf[AnyRef].eq(targetTuple.asInstanceOf[AnyRef]),
        repeatedSuccess.map(_.nonEmpty),
        repeatedFailure.map(_.isEmpty)
      )

    assertEquals(evidence, (Right(true), Right(true), true, true, Right(true), Right(true)))

  test("canonical-only binding rejects aliases, arbitrary constructors, duplicates, and missing names"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val aliasEnvironment = GlobalSelectedTypeEnvironment
        .fromWitnesses(using q)(TypeRepr.of[TopAlias])
        .toOption
        .get
      val aliasSpelling = GlobalSelectedTypeFrontend.construct(using q)(
        "quasiquotes.types.phase119.TopAlias",
        aliasEnvironment
      )
      val underlyingCanonical = GlobalSelectedTypeFrontend.construct(using q)(
        "quasiquotes.types.phase119.TopLevel",
        aliasEnvironment
      )

      val fakeEnvironment = GlobalSelectedTypeEnvironment
        .fromWitnesses(using q)(
          TypeRepr.of[UserConstructors.List[Int]],
          TypeRepr.of[UserConstructors.Box[Int]]
        )
        .toOption
        .get
      val fakeList = GlobalSelectedTypeFrontend.construct(using q)(
        "quasiquotes.types.phase119.UserConstructors.List[Int]",
        fakeEnvironment
      )
      val arbitrary = GlobalSelectedTypeFrontend.construct(using q)(
        "quasiquotes.types.phase119.UserConstructors.Box[Int]",
        fakeEnvironment
      )
      val duplicate = GlobalSelectedTypeEnvironment.fromWitnesses(using q)(
        TypeRepr.of[TopLevel],
        TypeRepr.of[TopLevel]
      )
      val missing = GlobalSelectedTypeFrontend.construct(using q)(
        "quasiquotes.types.phase119.OwnerOne.Nested",
        aliasEnvironment
      )
      val local = ResolvedTypeReflection.deriveFromOwner(using q)(
        Symbol.spliceOwner,
        "LocalOwnerType"
      )
      val wrongArityEnvironment = GlobalSelectedTypeEnvironment
        .fromWitnesses(using q)(TypeRepr.of[List[Int]])
        .toOption
        .get
      val wrongArity = GlobalSelectedTypeFrontend.construct(using q)(
        "scala.collection.immutable.List[Int, String]",
        wrongArityEnvironment
      )
      val extraBinding = GlobalSelectedTypeFrontend.construct(using q)(
        "quasiquotes.types.phase119.TopLevel",
        aliasEnvironment,
        "unused" -> TypeRepr.of[TopLevel]
      )
      (
        aliasSpelling.swap.toOption.map(_.message),
        underlyingCanonical.isRight,
        fakeList.swap.toOption.map(_.message),
        arbitrary.swap.toOption.map(_.message),
        duplicate.swap.toOption.map(_.message),
        missing.swap.toOption.map(_.message),
        local.swap.toOption.map(_.message),
        wrongArity.swap.toOption.map(_.message),
        extraBinding.swap.toOption.map(_.message)
      )

    assert(evidence._1.exists(_.startsWith("TYPE_NAME_RESOLUTION_UNRESOLVED")))
    assert(evidence._2)
    assert(evidence._3.exists(_.startsWith("TYPE_NAME_RESOLUTION_CONSTRUCTOR_POLICY_MISMATCH")))
    assert(evidence._4.exists(_.startsWith("TYPE_NAME_RESOLUTION_CONSTRUCTOR_POLICY_MISMATCH")))
    assert(evidence._5.exists(_.startsWith("TYPE_NAME_RESOLUTION_AMBIGUOUS")))
    assert(evidence._6.exists(_.startsWith("TYPE_NAME_RESOLUTION_UNRESOLVED")))
    assert(evidence._7.exists(_.startsWith("TYPE_NAME_RESOLVED_FAMILY_UNSUPPORTED")), evidence._7)
    assert(evidence._8.exists(_.startsWith("TYPE_NAME_RESOLUTION_CONSTRUCTOR_POLICY_MISMATCH")), evidence._8)
    assert(evidence._9.exists(_.startsWith("Unexpected type-construction binding(s)")), evidence._9)

  test("stable-term path targets fail closed and no-environment selected syntax remains unchanged"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val environment = GlobalSelectedTypeEnvironment
        .fromWitnesses(using q)(TypeRepr.of[TopLevel])
        .toOption
        .get
      val hole = GlobalSelectedTypeFrontend
        .compilePattern(using q)("$member", environment)
        .toOption
        .get
      val stable = GlobalSelectedTypeFrontend.matchPattern(using q)(
        hole,
        TypeRepr.of[StablePaths.first.Member],
        environment
      )
      val noEnvironment = TypeNormalFormSource.fromSource(
        "quasiquotes.types.phase119.TopLevel"
      )
      (
        stable.swap.toOption.map(_.message),
        noEnvironment.swap.toOption.map(_.message)
      )

    assert(evidence._1.exists(_.startsWith("TYPE_NAME_RESOLUTION_UNSTABLE_TERM_PREFIX")))
    assert(evidence._2.exists(_.startsWith("Selected type syntax")))
    assert(
      typeErrorMessages(
        "type MutableMember = quasiquotes.types.phase119.MutablePaths.current.Member"
      ).nonEmpty
    )

  test("selected terminals recurse through fixed forms and ordinary Type holes"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val nested = TypeRepr.of[OwnerOne.Nested]
      val environment = GlobalSelectedTypeEnvironment
        .fromWitnesses(using q)(
          TypeRepr.of[Int],
          TypeRepr.of[TopLevel],
          nested,
          TypeRepr.of[List[Int]],
          TypeRepr.of[Option[Int]],
          TypeRepr.of[Either[Int, String]]
        )
        .toOption
        .get
      val selectedInt = GlobalSelectedTypeFrontend.construct(using q)(
        "scala.Int",
        environment
      )
      val selectedNested = GlobalSelectedTypeFrontend.construct(using q)(
        "quasiquotes.types.phase119.OwnerOne.Nested",
        environment
      )
      val selectedHole = GlobalSelectedTypeFrontend.construct(using q)(
        "scala.Option[$value]",
        environment,
        "value" -> TypeRepr.of[TopLevel]
      )
      val fixedAroundSelected = GlobalSelectedTypeFrontend.construct(using q)(
        "Either[quasiquotes.types.phase119.TopLevel, scala.collection.immutable.List[quasiquotes.types.phase119.OwnerOne.Nested]]",
        environment
      )
      (
        selectedInt.map(_.asInstanceOf[AnyRef].eq(TypeRepr.of[Int].asInstanceOf[AnyRef])),
        selectedNested.map(_.asInstanceOf[AnyRef].eq(nested.asInstanceOf[AnyRef])),
        selectedHole.flatMap(TargetTypeReprInspector.inspectResolved(using q)(_, environment)).map(_.render),
        fixedAroundSelected.flatMap(TargetTypeReprInspector.inspectResolved(using q)(_, environment)).map(_.render)
      )

    assertEquals(evidence._1, Right(true))
    assertEquals(evidence._2, Right(true))
    assert(evidence._3.exists(_.contains("::TopLevel")), evidence._3)
    assert(evidence._4.exists(_.contains("::Nested")), evidence._4)
