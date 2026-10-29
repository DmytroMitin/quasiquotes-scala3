package quasiquotes.definitions.dotty

import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import dotty.tools.dotc.{Compiler, Driver}
import dotty.tools.dotc.ast.{untpd,tpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parser

/** All target identity observations below come from ordinary Typer/runtime. */
final class QuotedQualifierLexicalRootedTyperRuntimeTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*
  import QuotedRootedQualifierAuthorityModel.*

  Vector("unshadowed", "owner", "importAlias", "inherited", "enclosing", "local", "pathSegments").foreach { environment =>
    test(s"$environment lexical and genuinely rooted calls compile emit TASTy and execute"):
      val temp = Files.createTempDirectory("quoted-qualifier-context-")
      try
        val source = temp.resolve("Contexts.scala")
        val output = Files.createDirectory(temp.resolve("classes"))
        val args = Array("-classpath", compilationClasspath, "-d", output.toString, source.toString)
        // Control: unchanged AnyVal methods cannot satisfy the downstream Int use.
        Files.writeString(source, program(environment, "original"))
        val baseline = new Driver().process(args)
        assert(baseline.hasErrors)
        assert(baseline.allErrors.exists(_.message.contains("AnyVal")))
        Vector("lexicalSource", "rootedSource", "lexicalRewrite", "rootedPrivate").foreach { mode =>
          delete(output); Files.createDirectory(output)
          Files.writeString(source, program(environment, mode))
          val driver = new ContextDriver(mode)
          val reporter = driver.process(args)
          assert(!reporter.hasErrors, clues(environment,mode,reporter.allErrors.map(_.message)))
          if mode == "lexicalRewrite" || mode == "rootedPrivate" then
            assertEquals(driver.rewritten.sorted, Vector("Magnitudes", "Service"))
            assertEquals(driver.contexts.size, 2)
            assert(driver.clean)
            assertEquals(driver.fChecks, Vector.fill(2)(if environment == "owner" then "DIRECT_QUALIFIER_SHADOW" else "ORIGINAL_CONTEXT_QUALIFIER_UNPROVEN"))
          val files = emitted(output)
          Vector("Service", "Magnitudes").foreach { name =>
            assert(files.exists(p => p.contains(name) && p.endsWith(".class")), clues(files))
            // Local classes' TASTy belongs to their enclosing compilation class.
            if environment != "local" && environment != "enclosing" then assert(files.exists(_.endsWith(name+".tasty")), clues(files))
          }
          assert(files.exists(_.endsWith("Use.tasty")), clues(files))
          if environment == "enclosing" then assert(files.exists(_.endsWith("Container.tasty")),clues(files))
          val loader = new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
          try
            val cls = loader.loadClass("qualifier.runtime.Use$")
            val module = cls.getField("MODULE$").get(null)
            val canonical = mode.startsWith("rooted") || environment == "unshadowed" || environment == "pathSegments"
            val expected = if canonical then Vector(11,17) else Vector(99,99)
            val actual = Vector("echoValue","magnitudeValue").map(n => cls.getMethod(n).invoke(module).asInstanceOf[Integer].intValue)
            assertEquals(actual,expected,clues(environment,mode))
            val expectedTargets = if canonical then Vector("java.lang.Math","java.lang.StrictMath") else Vector.fill(2)("qualifier.runtime.LocalAbs")
            assertEquals(driver.targets.sorted,expectedTargets.sorted,clues(environment,mode))
            assertEquals(cls.getMethod("siblings").invoke(module), Integer.valueOf(10))
            println(s"QUALIFIER_MATRIX environment=$environment mode=$mode runtime=${actual.mkString("/")} contexts=${driver.contexts.mkString(";")} F=${driver.fChecks.mkString(",")} targets=${driver.targets.mkString(",")}")
          finally loader.close()
        }
      finally delete(temp)
  }

  private final class ContextDriver(mode: String) extends Driver:
    var rewritten = Vector.empty[String]
    var contexts = Vector.empty[String]
    var fChecks = Vector.empty[String]
    var clean = true
    var targets = Vector.empty[String]
    override protected def newCompiler(using Context): Compiler = new Compiler:
      override protected def frontendPhases: List[List[Phase]] =
        val phases = if mode == "lexicalRewrite" || mode == "rootedPrivate" then
          List(new Parser) :: List(new Rewrite(ContextDriver.this, mode)) :: super.frontendPhases.tail
        else super.frontendPhases
        phases :+ List(new ObserveTargets(ContextDriver.this))

  private final class ObserveTargets(driver: ContextDriver) extends Phase:
    def phaseName: String = "observeActualTypedCallTargets"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val traverser = new tpd.TreeTraverser:
        def traverse(tree: tpd.Tree)(using Context): Unit =
          tree match
            case a: tpd.Apply if a.fun.symbol != NoSymbol && a.fun.symbol.name.toString == "abs" =>
              driver.targets :+= a.fun.symbol.owner.fullName.toString.stripSuffix("$")
            case _ => ()
          traverseChildren(tree)
      traverser.traverse(summon[Context].compilationUnit.tpdTree)

  private final class Rewrite(driver: ContextDriver, mode: String) extends Phase:
    def phaseName: String = "quotedQualifierContextProof"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val mapper = new untpd.UntypedTreeMap:
        override def transform(tree: untpd.Tree)(using ctx: Context): untpd.Tree = tree match
          case root: untpd.TypeDef if Set("Service","Magnitudes")(root.name.toString) =>
            val name = if root.name.toString == "Service" then "echo" else "magnitude"
            val qualifier = if name == "echo" then "Math" else "StrictMath"
            val capture = right(ExistingClassUntypedRewrite.capture(root))
            val selected = right(QuotedExistingMethodAuthorityModel.select(capture,name))
            assert(root.symbol == NoSymbol)
            val ownerName = if ctx.owner == NoSymbol then "NoSymbol" else ctx.owner.name.toString
            assert(!Set("Service","Magnitudes")(ownerName))
            driver.contexts :+= s"owner=$ownerName,scopeSize=${ctx.scope.size},importInfo=${ctx.importInfo != null},rawOwnerSymbol=NoSymbol"
            driver.fChecks :+= canonicalContextCheck(selected,qualifier).left.toOption.get.code
            val old = root.rhs.asInstanceOf[untpd.Template]
            val index = selected.member.index
            val originalMethod = old.body(index).asInstanceOf[untpd.DefDef]
            val originalGraph = ExistingUntpdClassMemberFilter.allTrees(root).map(t => (t,t.source,t.span))
            val result = if mode == "lexicalRewrite" then
              val quote = right(QuotedExistingMethodAuthorityModel.quotedReplacement(selected))
              right(QuotedExistingMethodAuthorityModel.replace(selected,quote)).tree
            else
              val quoted = right(quote(selected))
              val rejected = QuotedExistingMethodAuthorityModel.replace(selected,quoted).left.toOption.get
              assert(rejected.detail.contains("SEMANTIC_APPLY_FUNCTION_UNSUPPORTED"), clues(rejected))
              val fragment = right(prepare(selected))
              right(rewrite(selected,fragment))
            assertEquals(ExistingUntpdClassMemberFilter.allTrees(root).map(t => (t,t.source,t.span)), originalGraph)
            val fresh = result.rhs.asInstanceOf[untpd.Template]
            assert(!result.eq(root)); assert(!fresh.eq(old))
            assertEquals((result.source,result.span),(root.source,root.span))
            assertEquals((fresh.source,fresh.span),(old.source,old.span))
            assertEquals(fresh.body.size,old.body.size)
            old.body.indices.foreach(i => assertEquals(fresh.body(i).eq(old.body(i)),i != index))
            val changed = fresh.body(index).asInstanceOf[untpd.DefDef]
            assertEquals((changed.source,changed.span),(originalMethod.source,originalMethod.span))
            assertEquals((changed.rhs.source,changed.rhs.span),(originalMethod.rhs.source,originalMethod.rhs.span))
            val beforeParam = originalMethod.paramss.head.head
            val afterParam = changed.paramss.head.head
            assert(!afterParam.eq(beforeParam))
            assertEquals((afterParam.source,afterParam.span),(beforeParam.source,beforeParam.span))
            driver.clean &&= ExistingUntpdClassMemberFilter.allTrees(result).forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice])
            driver.rewritten :+= root.name.toString
            result
          case other => super.transform(other)
      val unit = summon[Context].compilationUnit
      unit.untpdTree = mapper.transform(unit.untpdTree)

  private def program(environment: String, mode: String): String =
    val original = mode == "original" || mode == "lexicalRewrite" || mode == "rootedPrivate"
    val tpe = if original then "AnyVal" else "Int"
    val prefix = if mode == "rootedSource" then "_root_.java.lang." else ""
    val echo = if original then "value" else s"${prefix}Math.abs(value)"
    val magnitude = if original then "number" else s"${prefix}StrictMath.abs(number)"
    val fields = "val Math: LocalAbs = new LocalAbs; val StrictMath: LocalAbs = new LocalAbs;"
    val own = if environment == "owner" then fields else if environment == "pathSegments" then "val java: LocalAbs = new LocalAbs; val lang: LocalAbs = new LocalAbs;" else ""
    val parent = if environment == "inherited" then " extends Parent" else ""
    val classes = s"""class Service$parent { $own val before: Int = 1; def echo(value: $tpe): $tpe = $echo; val after: Int = 2 }
      |class Magnitudes$parent { $own def magnitude(number: $tpe): $tpe = $magnitude; val untouched: Int = 7 }
      |""".stripMargin
    val methods = """def echoValue: Int = new Service().echo(-11)
      |def magnitudeValue: Int = new Magnitudes().magnitude(-17)
      |def siblings: Int = { val s = new Service(); s.before + s.after + new Magnitudes().untouched }
      |""".stripMargin
    val body = environment match
      case "enclosing" => s"object Container { $fields $classes }; object Use { import Container.*; $methods }"
      case "local" => s"""object Use {
        |def values: (Int,Int,Int) = { $fields $classes
        |val s = new Service(); val m = new Magnitudes(); (s.echo(-11),m.magnitude(-17),s.before+s.after+m.untouched) }
        |def echoValue: Int = values._1
        |def magnitudeValue: Int = values._2
        |def siblings: Int = values._3 }
        |""".stripMargin
      case _ => s"$classes object Use { $methods }"
    val imports = if environment == "importAlias" then "object Aliases { val math: LocalAbs = new LocalAbs; val strict: LocalAbs = new LocalAbs }; import Aliases.{math => Math, strict => StrictMath};" else ""
    s"package qualifier.runtime\nclass LocalAbs { def abs(value: Int): Int = 99 }; class Parent { $fields }; $imports\n$body"

  private def right[E,A](v: Either[E,A]): A = v.fold(p => fail(p.toString),identity)
  private def compilationClasspath: String =
    Vector(classOf[scala.Option[?]],classOf[scala.deriving.Mirror],classOf[Compiler],classOf[ExistingClassUntypedRewrite.type],getClass)
      .flatMap(c => Option(c.getProtectionDomain).flatMap(d => Option(d.getCodeSource)).map(_.getLocation.toURI))
      .map(Path.of(_).toString).distinct.mkString(java.io.File.pathSeparator)
  private def emitted(root: Path): Vector[String] =
    val stream = Files.walk(root)
    try stream.filter(Files.isRegularFile(_)).iterator.asScala.map(_.toString).toVector
    finally stream.close()
  private def delete(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try stream.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally stream.close()
