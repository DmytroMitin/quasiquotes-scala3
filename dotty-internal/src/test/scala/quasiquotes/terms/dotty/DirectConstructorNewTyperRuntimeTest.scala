package quasiquotes.terms.dotty

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.{Compiler, Driver, report}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.util.{SourceFile, Spans}

import quasiquotes.parser.TermShape

class DirectConstructorNewTyperRuntimeTest extends munit.FunSuite:
  test("direct source-free constructor survives pre-Typer TASTy class emission and runtime") {
    val temporary = Files.createTempDirectory("u009-direct-constructor-")
    try
      val source = temporary.resolve("U009DirectConstructorRuntime.scala")
      val output = temporary.resolve("classes")
      Files.createDirectories(output)
      Files.writeString(
        source,
        """object U009DirectConstructorRuntime:
          |  def result: String = "placeholder"
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val driver = new DirectConstructorDriver
      val reporter =
        driver.process(
          Array(
            "-classpath",
            compilationClasspath,
            "-d",
            output.toString,
            source.toString
          )
        )

      assert(!reporter.hasErrors, clues(reporter.allErrors))
      assertEquals(driver.insertedSourceFree, Some(true))
      assertEquals(driver.insertedConstructorTopology, Some(true))

      val emitted =
        val stream = Files.walk(output)
        try stream.filter(Files.isRegularFile(_)).iterator().asScala.toVector
        finally stream.close()
      assert(emitted.exists(_.toString.endsWith(".class")))
      assert(emitted.exists(_.toString.endsWith(".tasty")))

      val loader =
        new URLClassLoader(Array(output.toUri.toURL), getClass.getClassLoader)
      try
        val moduleClass = loader.loadClass("U009DirectConstructorRuntime$")
        val module = moduleClass.getField("MODULE$").get(null)
        val value = moduleClass.getMethod("result").invoke(module)
        assertEquals(value, "u009-direct")
      finally loader.close()
    finally deleteRecursively(temporary)
  }

  private final class DirectConstructorDriver extends Driver:
    @volatile var insertedSourceFree: Option[Boolean] = None
    @volatile var insertedConstructorTopology: Option[Boolean] = None

    override protected def newCompiler(using Context): Compiler =
      new Compiler:
        override protected def frontendPhases: List[List[Phase]] =
          List(new Parser) ::
            List(new InsertDirectConstructor(DirectConstructorDriver.this)) ::
            super.frontendPhases.tail

  private final class InsertDirectConstructor(evidence: DirectConstructorDriver)
      extends Phase:
    def phaseName: String = "u009DirectConstructorInsertion"
    override def isCheckable: Boolean = false

    protected def run(using Context): Unit =
      CoreTermShapeUntypedLowerer.lower(runtimeFixture) match
        case Left(error) => report.error(error.message)
        case Right(raw) =>
          CoreTermShapeUntypedLowerer.verifySourceFreeForTest(raw) match
            case Left(error) => report.error(error.message)
            case Right(()) =>
              evidence.insertedConstructorTopology = Some(
                raw match
                  case untpd.Apply(
                        untpd.Select(
                          untpd.Apply(
                            untpd.Select(_: untpd.New, constructor),
                            List(_: untpd.Literal)
                          ),
                          method
                        ),
                        Nil
                      ) =>
                    constructor.toString == "<init>" &&
                      method.toString == "toString"
                  case _ => false
              )
              var replacementSite: Option[(SourceFile, Spans.Span)] = None
              val insertion = new untpd.UntypedTreeMap:
                override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
                  tree match
                    case definition @ untpd.DefDef(name, paramss, tpt, _)
                        if name.toString == "result" =>
                      replacementSite =
                        Some(definition.rhs.source -> definition.rhs.span)
                      untpd.cpy.DefDef(definition)(name, paramss, tpt, raw)
                    case _ => super.transform(tree)
              val context = summon[Context]
              context.compilationUnit.untpdTree =
                insertion.transform(context.compilationUnit.untpdTree)
              CoreTermShapeUntypedLowerer.verifySourceFreeForTest(raw) match
                case Left(error) => report.error(error.message)
                case Right(()) =>
                  evidence.insertedSourceFree = Some(true)
                  replacementSite match
                    case None => report.error("U009 result replacement site was not found")
                    case Some((source, span)) =>
                      val positioned = positionEveryNode(raw, source, span)
                      val positioning = new untpd.UntypedTreeMap:
                        override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
                          if tree.eq(raw) then positioned else super.transform(tree)
                      context.compilationUnit.untpdTree =
                        positioning.transform(context.compilationUnit.untpdTree)

  private def positionEveryNode(
      tree: untpd.Tree,
      source: SourceFile,
      span: Spans.Span
  )(using Context): untpd.Tree =
    val positioner = new untpd.UntypedTreeMap:
      override def transform(current: untpd.Tree)(using Context): untpd.Tree =
        super.transform(current).cloneIn(source).withSpan(span)
    positioner.transform(tree)

  private def runtimeFixture: TermShape =
    TermShape.Apply(
      TermShape.Select(
        TermShape.New(
          "java.lang.StringBuilder",
          List(TermShape.Literal("\"u009-direct\""))
        ),
        "toString"
      ),
      Nil
    )

  private def compilationClasspath: String =
    Vector(
      classOf[scala.Option[?]],
      classOf[scala.deriving.Mirror],
      classOf[dotty.tools.dotc.Compiler],
      getClass
    )
      .flatMap(value =>
        Option(value.getProtectionDomain)
          .flatMap(domain => Option(domain.getCodeSource))
          .map(_.getLocation.toURI)
      )
      .map(Path.of(_).toString)
      .distinct
      .mkString(java.io.File.pathSeparator)

  private def deleteRecursively(root: Path): Unit =
    if Files.exists(root) then
      val stream = Files.walk(root)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.deleteIfExists(_))
      finally stream.close()
