package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaTypeAuthoringAstCharacterizationTest extends munit.FunSuite:
  test("direct Type constructors preserve the admitted topology and child order"):
    val authored = Type.Function(
      List(
        Type.Apply(Type.Name("List"), List(Type.Name("Int"))),
        Type.Tuple(List(Type.Name("String"), Type.Name("Boolean")))
      ),
      Type.Apply(Type.Name("Option"), List(Type.Name("AnyVal")))
    )

    assertEquals(authored.productPrefix, "Type.Function")
    assertEquals(authored.params.map(_.productPrefix), List("Type.Apply", "Type.Tuple"))

    val list = authored.params.head.asInstanceOf[Type.Apply]
    assertEquals(list.tpe.asInstanceOf[Type.Name].value, "List")
    assertEquals(list.args.map(_.asInstanceOf[Type.Name].value), List("Int"))

    val tuple = authored.params(1).asInstanceOf[Type.Tuple]
    assertEquals(tuple.args.map(_.asInstanceOf[Type.Name].value), List("String", "Boolean"))

    val option = authored.res.asInstanceOf[Type.Apply]
    assertEquals(option.tpe.asInstanceOf[Type.Name].value, "Option")
    assertEquals(option.args.map(_.asInstanceOf[Type.Name].value), List("AnyVal"))

  test("directly constructed Type roots children and synthetic clauses are unpositioned"):
    val authored = Type.Function(
      List(Type.Apply(Type.Name("Either"), List(Type.Name("Int"), Type.Name("String")))),
      Type.Tuple(List(Type.Name("Boolean"), Type.Name("AnyVal")))
    )

    assert(allTypeNodes(authored).forall(_.pos == Position.None))
    val application = authored.params.head.asInstanceOf[Type.Apply]
    assertEquals(application.argClause.pos, Position.None)
    assertEquals(authored.paramClause.pos, Position.None)

  test("unpositioned constructors synthesize printable tokens without source provenance"):
    val authored = Type.Apply(
      Type.Name("Either"),
      List(Type.Name("Int"), Type.Name("String"))
    )

    assertEquals(authored.pos, Position.None)
    assertEquals(authored.tokens.map(_.text).mkString, "Either[Int, String]")

  private def allTypeNodes(root: Type): List[Type] =
    root :: (root match
      case _: Type.Name => Nil
      case applied: Type.Apply =>
        allTypeNodes(applied.tpe) ++ applied.args.flatMap(allTypeNodes)
      case tuple: Type.Tuple =>
        tuple.args.flatMap(allTypeNodes)
      case function: Type.Function =>
        function.params.flatMap(allTypeNodes) ++ allTypeNodes(function.res)
      case other => fail(s"unexpected characterization node: ${other.productPrefix}")
    )
