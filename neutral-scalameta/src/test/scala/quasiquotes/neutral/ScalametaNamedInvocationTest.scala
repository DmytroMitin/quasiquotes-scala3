package quasiquotes.neutral

import scala.meta.*
import scala.meta.dialects.Scala3
import _root_.quasiquotes.parser.TermShape
import scala.compiletime.testing.typeCheckErrors

final class ScalametaNamedInvocationTest extends munit.FunSuite:
  import ScalametaNamedInvocation.*
  private def parse(s: String): Term = Input.String(s).parse[Term].get
  private def nodes(t: Tree): List[Tree] = t :: t.children.flatMap(nodes)
  test("named Apply and New retain role labels order and fresh AST structure"):
    for n <- List(1,2,3,8); prefix <- List("f", "new Box") do
      val s = prefix + (1 to n).map(i => s"p$i = g($i)").mkString("(",", ",")")
      val original = parse(s)
      val p = project(original).toOption.get
      assertEquals(p.invocation.arguments.map(_.label.source),(1 to n).map(i => s"p$i").toList)
      assertEquals(p.invocation.kind,if prefix == "f" then Kind.Apply else Kind.New)
      val fresh = author(p.invocation).toOption.get
      assertEquals(fresh.structure, original.structure)
      assertEquals(project(fresh).toOption.get.invocation,p.invocation)
      assert(nodes(fresh).forall(_.pos == Position.None))
      assert(!nodes(fresh).exists(t => nodes(original).exists(_ eq t)))
      assertEquals(project(fresh).toOption.get.source.span,None)
      assertEquals(p.source.tokens,s)
      p.arguments.foreach(o => assertEquals(s.substring(o.span.get._1,o.span.get._2),o.tokens))
  test("role erasure collisions are not exposed to legacy Core"):
    val a=project(parse("f(x = 1, y = 1)")).toOption.get.invocation
    val b=project(parse("f(y = 1, x = 1)")).toOption.get.invocation
    assertEquals(a.arguments.map(_.value),b.arguments.map(_.value))
    assertNotEquals(a,b)
    assertEquals(toLegacy(a).left.toOption.map(_.code),Some("NAMED_INVOCATION_NOT_LEGACY_TERMSHAPE"))
    val errors=typeCheckErrors("val x: _root_.quasiquotes.parser.TermShape = null.asInstanceOf[_root_.quasiquotes.neutral.ScalametaNamedInvocation.Invocation]")
    println("N085_NO_CONVERSION " + errors)
    assert(errors.exists(e => e.message.contains("Required") && e.message.contains("Found")),clues(errors))
    List("f(x = 1)","new Box(x = 1)").foreach(s => assert(ScalametaTermProjection.project(parse(s)).isLeft))
    List("f()","f(1)","f(1,2,3)","new Box()","new Box(1)","new Box(1,2,3)","f()(1)(2)").foreach { s =>
      val shape=ScalametaTermProjection.project(parse(s)).toOption.get.shape
      assert(ScalametaTermShapeAuthoring.author(shape).isRight)
      assert(shape.render.nonEmpty)
    }
  test("invalid source contexts reject with stable private diagnostics"):
    val cases=List("x = 1"->"INVOCATION_CONTEXT_REQUIRED", "f()"->"NAMED_ONLY_REQUIRED", "new Box()"->"NAMED_ONLY_REQUIRED",
      "f(1)"->"NAMED_ONLY_REQUIRED", "f(x=1,2)"->"NAMED_ONLY_REQUIRED", "f(x=1)(y=2)"->"CLAUSE_COUNT_UNSUPPORTED",
      "new Box(x=1)(y=2)"->"CLAUSE_COUNT_UNSUPPORTED", "f(using x=1)"->"CONTEXTUAL_UNSUPPORTED",
      "f[Int](x=1)"->"TARGET_UNSUPPORTED", "f(x=1,x=2)"->"DUPLICATE_LABEL",
      "f(`x`=1)"->"LABEL_UNSUPPORTED", "obj.f(x=1)"->"TARGET_UNSUPPORTED", "`f`(x=1)"->"TARGET_UNSUPPORTED",
      "new pkg.Box(x=1)"->"TARGET_UNSUPPORTED", "new `Box`(x=1)"->"TARGET_UNSUPPORTED",
      "f(x = (a: Int) => a)"->"RHS_UNSUPPORTED", "f(x = g(y=1))"->"RHS_UNSUPPORTED", "f(x = xs*)"->"RHS_UNSUPPORTED")
    cases.foreach { (s,code) => assertEquals(project(parse(s)).left.toOption.map(_.code),Some(code),clues(s)) }
    assertEquals(project(null).left.toOption.map(_.code),Some("MISSING_INVOCATION"))
  test("checked factories and JVM reflective construction cannot admit malformed values"):
    val one=named("x",TermShape.Literal("1")).toOption.get
    assert(named(null,TermShape.Literal("1")).isLeft)
    assert(named("`type`",TermShape.Literal("1")).isLeft)
    assert(named("x",null).isLeft)
    assert(named("x",TermShape.Apply(null,null)).isLeft)
    assert(invocation(null,"f",List(one)).isLeft)
    assert(invocation(Kind.Apply,null,List(one)).isLeft)
    assert(invocation(Kind.Apply,"f",null).isLeft)
    assert(invocation(Kind.Apply,"f",Nil).isLeft)
    assert(invocation(Kind.Apply,"f",List(null)).isLeft)
    assert(invocation(Kind.Apply,"f",List(one,one)).isLeft)
    assert(author(null).isLeft)
    List(classOf[Named],classOf[Invocation]).foreach { cls =>
      cls.getDeclaredConstructors.foreach { c =>
        c.setAccessible(true)
        val e=intercept[java.lang.reflect.InvocationTargetException] { c.newInstance(Array.fill[AnyRef](c.getParameterCount)(null)*) }
        assert(e.getCause.isInstanceOf[IllegalArgumentException])
      }
    }
  test("fresh provenance is not source replay or authority"):
    val p=project(parse("f(/* note */ x = 1)")).toOption.get
    assert(p.source.tokens.contains("note"))
    assert(!author(p.invocation).toOption.get.syntax.contains("note"))
  test("delegated binder-free fragments keep their existing ordinary admission"):
    List("value", "obj.value", "new Box(1)", "a + b", "!flag", "(1: Int)", "(1,2)",
      "if (flag) 1 else 2", "s\"hello $name\"", "g()(1)(2)").foreach { rhs =>
      val original=parse("f(x = " + rhs + ")")
      val p=project(original).fold(e => fail(e.toString),identity)
      assertEquals(author(p.invocation).toOption.get.structure,original.structure,clues(rhs))
    }
    val rhs=TermShape.Literal("1")
    assert(named("x",rhs).toOption.get.value eq rhs)
  test("Java-forged kind equality cannot bypass factory or constructor invariants"):
    val stream = getClass.getResourceAsStream("/named-invocation-kind-forgery.class")
    val bytes = try stream.readAllBytes() finally stream.close()
    val parent = getClass.getClassLoader
    class Loader extends ClassLoader(parent):
      def instance(): AnyRef = defineClass(null,bytes,0,bytes.length).getConstructor().newInstance()
    val forged = new Loader().instance().asInstanceOf[Kind]
    val arg = named("x",TermShape.Literal("1")).toOption.get
    assert(invocation(forged,"f",List(arg)).isLeft)
    val target = _root_.quasiquotes.definitions.DefinitionName.fromSource("f").toOption.get
    val ctor = classOf[Invocation].getDeclaredConstructors.find(_.getParameterCount == 3).get
    ctor.setAccessible(true)
    val error = intercept[java.lang.reflect.InvocationTargetException] { ctor.newInstance(forged,target,List(arg)) }
    assert(error.getCause.isInstanceOf[IllegalArgumentException])
