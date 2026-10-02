package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaExtensionModuleCharacterizationTest extends munit.FunSuite:
  private val Canonical =
    """object syntax:
      |  extension [A](a: A)
      |    def combine(a1: A)(using inst: Monoid[A]): A =
      |      inst.combine(a, a1)""".stripMargin

  test("parsed canonical and renamed fixtures have the exact extension-module topology"):
    val rows = List(
      parse(Canonical) -> Names("syntax", "A", "a", "combine", "a1", "inst", "Monoid"),
      parse(
        """object operations:
          |  extension [Element](left: Element)
          |    def merge(right: Element)(using evidence: Combine[Element]): Element =
          |      evidence.merge(left, right)""".stripMargin
      ) -> Names("operations", "Element", "left", "merge", "right", "evidence", "Combine")
    )

    rows.foreach { case (definition, names) => assertExactTopology(definition, names) }

  test("direct constructors reproduce parsed structure and remain wholly unpositioned"):
    val names = Names("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    val direct = construct(names)

    assertEquals(direct.structure, parse(Canonical).structure)
    assertExactTopology(direct, names)
    assert(allTrees(direct).forall(_.pos == Position.None))

  private final case class Names(
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      ordinary: String,
      contextual: String,
      evidenceConstructor: String
  )

  private def construct(names: Names): Defn.Object =
    val typeParameter = Type.Param(
      Nil,
      Type.Name(names.typeParameter),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )
    val receiver = Term.Param(
      Nil,
      Term.Name(names.receiver),
      Some(Type.Name(names.typeParameter)),
      None
    )
    val ordinary = Term.Param(
      Nil,
      Term.Name(names.ordinary),
      Some(Type.Name(names.typeParameter)),
      None
    )
    val contextual = Term.Param(
      List(Mod.Using()),
      Term.Name(names.contextual),
      Some(
        Type.Apply(
          Type.Name(names.evidenceConstructor),
          Type.ArgClause(List(Type.Name(names.typeParameter)))
        )
      ),
      None
    )
    val method = Defn.Def(
      Nil,
      Term.Name(names.method),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(List(ordinary)),
            Term.ParamClause(List(contextual), Some(Mod.Using()))
          )
        )
      ),
      Some(Type.Name(names.typeParameter)),
      Term.Apply(
        Term.Select(Term.Name(names.contextual), Term.Name(names.method)),
        Term.ArgClause(List(Term.Name(names.receiver), Term.Name(names.ordinary)))
      )
    )
    val extension = Defn.ExtensionGroup(
      Some(
        Member.ParamClauseGroup(
          Type.ParamClause(List(typeParameter)),
          List(Term.ParamClause(List(receiver)))
        )
      ),
      method
    )
    Defn.Object(
      Nil,
      Term.Name(names.module),
      Template(
        Nil,
        Nil,
        Self(Name.Anonymous(), None),
        List(extension),
        Nil
      )
    )

  private def assertExactTopology(definition: Defn.Object, names: Names): Unit =
    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, names.module)
    assertEquals(definition.templ.early, Nil)
    assertEquals(definition.templ.inits, Nil)
    assert(definition.templ.self.isEmpty)
    assertEquals(definition.templ.derives, Nil)

    definition.templ.stats match
      case List(extension: Defn.ExtensionGroup) =>
        extension.paramClauseGroup match
          case Some(
                Member.ParamClauseGroup(
                  Type.ParamClause(List(typeParameter)),
                  List(receiverClause)
                )
              ) =>
            assertEquals(typeParameter.mods, Nil)
            assertEquals(typeParameter.name.value, names.typeParameter)
            assertEquals(typeParameter.tparamClause.values, Nil)
            assertEquals(typeParameter.bounds.lo, None)
            assertEquals(typeParameter.bounds.hi, None)
            assertEquals(typeParameter.bounds.context, Nil)
            assertEquals(typeParameter.bounds.view, Nil)
            assertEquals(receiverClause.mod, None)
            receiverClause.values match
              case List(receiver) =>
                assertEquals(receiver.mods, Nil)
                assertEquals(receiver.name.value, names.receiver)
                assertEquals(receiver.decltpe.map(_.structure), Some(Type.Name(names.typeParameter).structure))
                assertEquals(receiver.default, None)
              case other => fail(s"expected one extension receiver, found $other")
          case other => fail(s"expected one extension type parameter and receiver, found $other")

        extension.body match
          case method: Defn.Def => assertExactMethod(method, names)
          case other => fail(s"expected one nested Defn.Def, found ${other.productPrefix}")
      case other => fail(s"expected one Defn.ExtensionGroup, found $other")

  private def assertExactMethod(method: Defn.Def, names: Names): Unit =
    assertEquals(method.mods, Nil)
    assertEquals(method.name.value, names.method)
    method.paramClauseGroups match
      case List(
            Member.ParamClauseGroup(
              Type.ParamClause(Nil),
              List(ordinaryClause, contextualClause)
            )
          ) =>
        assertEquals(ordinaryClause.mod, None)
        ordinaryClause.values match
          case List(ordinary) =>
            assertEquals(ordinary.mods, Nil)
            assertEquals(ordinary.name.value, names.ordinary)
            assertEquals(ordinary.decltpe.map(_.structure), Some(Type.Name(names.typeParameter).structure))
            assertEquals(ordinary.default, None)
          case other => fail(s"expected one ordinary parameter, found $other")

        assert(contextualClause.mod.exists(_.isInstanceOf[Mod.Using]))
        contextualClause.values match
          case List(contextual) =>
            assert(contextual.mods.forall(_.isInstanceOf[Mod.Using]))
            assertEquals(contextual.name.value, names.contextual)
            assertEquals(contextual.default, None)
            contextual.decltpe match
              case Some(Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
                assertEquals(constructor.value, names.evidenceConstructor)
                assertEquals(argument.value, names.typeParameter)
              case other => fail(s"expected direct unary evidence Type, found $other")
          case other => fail(s"expected one contextual parameter, found $other")
      case other => fail(s"expected empty method tparams and ordinary/using clauses, found $other")

    assertEquals(method.decltpe.map(_.structure), Some(Type.Name(names.typeParameter).structure))
    method.body match
      case Term.Apply(
            Term.Select(receiver: Term.Name, selected: Term.Name),
            List(first: Term.Name, second: Term.Name)
          ) =>
        assertEquals(receiver.value, names.contextual)
        assertEquals(selected.value, names.method)
        assertEquals(first.value, names.receiver)
        assertEquals(second.value, names.ordinary)
      case other => fail(s"expected evidence.method(receiver, ordinary), found ${other.productPrefix}")

  private def parse(source: String): Defn.Object =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Object => definition
      case other => fail(s"expected Defn.Object, found ${other.productPrefix}")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
