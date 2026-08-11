package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

class ConstructorNewRawPreflightTest extends munit.FunSuite:
  private val positives = Vector(
    "new java.lang.StringBuilder()",
    "new java.lang.StringBuilder(16)",
    "new java.lang.RuntimeException(\"boom\")",
    "new java.lang.StringBuilder(if cond then 8 else 16)",
    "new java.lang.StringBuilder(foo(x))",
    "(new java.lang.StringBuilder(16)).toString",
    "foo(new java.lang.StringBuilder(16))"
  )

  positives.foreach { source =>
    test(s"records exact raw constructor shape and spans for $source") {
      val (raw, sourceFile, context) = parse(source)
      given Context = context
      val summary = describe(raw, source)
      println(s"CONSTRUCTOR_NEW_RAW_PREFLIGHT source=$source sourceFile=${sourceFile.path} root=${raw.getClass.getName} tree=$summary")

      assertEquals(raw.span.start, 0)
      assertEquals(raw.span.end, source.length)
      assert(allTrees(raw).exists(_.isInstanceOf[untpd.New]))
      assert(allTrees(raw).exists {
        case untpd.Select(_: untpd.New, name) => name.toString == "<init>"
        case _ => false
      })
      allTrees(raw).foreach(tree => assertEquals(tree.symbol, NoSymbol))
    }
  }

  private val boundaries = Vector(
    "new StringBuilder(16)",
    "new java.lang.StringBuilder[Int](16)",
    "new java.lang.StringBuilder(16)(17)",
    "new java.lang.StringBuilder(capacity = 16)",
    "new java.lang.StringBuilder(16) { }",
    "new __qq_ctor_type_hole__(16)"
  )

  boundaries.foreach { source =>
    test(s"records raw constructor boundary shape for $source") {
      parseEither(source) match
        case Left(messages) =>
          println(s"CONSTRUCTOR_NEW_RAW_BOUNDARY source=$source parserErrors=${messages.mkString(" | ")}")
        case Right((raw, sourceFile, context)) =>
          given Context = context
          println(
            s"CONSTRUCTOR_NEW_RAW_BOUNDARY source=$source sourceFile=${sourceFile.path} " +
              s"root=${raw.getClass.getName} tree=${describe(raw, source)}"
          )
          allTrees(raw).foreach(tree => assertEquals(tree.symbol, NoSymbol))
      assert(true)
    }
  }

  private def parse(source: String): (untpd.Tree, SourceFile, Context) =
    parseEither(source).fold(messages => fail(messages.mkString("; ")), identity)

  private def parseEither(source: String): Either[List[String], (untpd.Tree, SourceFile, Context)] =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val sourceFile = SourceFile.virtual("ConstructorNewRawPreflight.scala", source)
    val parser = new Parser(sourceFile)
    val raw = parser.expr()
    val messages = reporter.pendingMessages.map(_.message).toList
    if messages.nonEmpty || parser.in.token != Tokens.EOF then
      Left(messages :+ s"remainingLexeme=${Tokens.tokenString(parser.in.token)}")
    else Right((raw, sourceFile, summon[Context]))

  private def describe(tree: untpd.Tree, source: String)(using Context): String =
    val span = tree.span
    val bounds = if span.exists then s"${span.start}..${span.point}..${span.end}" else "NoSpan"
    val slice = if span.exists && span.start >= 0 && span.end <= source.length then source.slice(span.start, span.end) else "<none>"
    val details = tree match
      case untpd.Ident(name) => s"name=${name.toString}"
      case untpd.Select(_, name) => s"name=${name.toString}"
      case untpd.Apply(_, arguments) => s"arguments=${arguments.size}"
      case untpd.New(_) => "genuineNew=true"
      case untpd.AppliedTypeTree(_, arguments) => s"typeArguments=${arguments.size}"
      case untpd.NamedArg(name, _) => s"namedArgument=${name.toString}"
      case _ => ""
    val children = directChildren(tree)
    s"${tree.getClass.getName}(span=$bounds,slice=$slice,noSymbol=${tree.symbol == NoSymbol},$details)" +
      (if children.isEmpty then "" else children.map(describe(_, source)).mkString("[", ",", "]"))

  private def directChildren(tree: untpd.Tree): Vector[untpd.Tree] =
    tree match
      case untpd.Select(qualifier, _) => Vector(qualifier)
      case untpd.Apply(function, arguments) => function +: arguments.toVector
      case untpd.New(typeTree) => Vector(typeTree)
      case untpd.AppliedTypeTree(constructor, arguments) => constructor +: arguments.toVector
      case untpd.NamedArg(_, value) => Vector(value)
      case untpd.If(condition, thenBranch, elseBranch) => Vector(condition, thenBranch, elseBranch)
      case untpd.Parens(expression) => Vector(expression)
      case untpd.Template(constr, _, self, _) =>
        Vector[untpd.Tree](constr, self)
      case _ => Vector.empty

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)
