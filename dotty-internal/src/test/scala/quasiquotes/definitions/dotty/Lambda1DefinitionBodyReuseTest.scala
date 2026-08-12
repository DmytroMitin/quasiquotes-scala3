package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName, DefinitionShape}
import quasiquotes.parser.{BinderId, TermShape, TypeShape}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class Lambda1DefinitionBodyReuseTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")
  private val functionType = TypeNormalForm.STypeFunction(List(intType), intType)

  test("completed definition adapters inherit exact Lambda1 body lowering") {
    val definition = completedDefinition
    withContext {
      val raw = ConstructedDefinitionUntypedBackend
        .lower(definition)
        .toOption
        .get
        .asInstanceOf[untpd.DefDef]

      assert(raw.rhs.isInstanceOf[untpd.Function])
      allTrees(raw).foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
      }

      val generated = ConstructedDefinitionGeneratedOriginAdapter
        .lower(definition, "<definition-lambda1-reuse>")
        .toOption
        .get
      assertEquals(
        generated.generatedSource,
        "def identity: Int => Int = (x: Int) => x"
      )
      assert(generated.tree.asInstanceOf[untpd.DefDef].rhs.isInstanceOf[untpd.Function])
      allTrees(generated.tree).foreach { tree =>
        assertEquals(tree.source.path, generated.sourceFile.path)
        assert(tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("DefinitionShape syntax remains explicitly narrower than completed-body reuse") {
    val binderId = BinderId(0)
    val shape = TermShape.Lambda1(
      binderId,
      "x",
      "Int",
      TermShape.BoundReference(binderId, "x")
    )

    assert(
      DefinitionShape
        .parameterlessDef(
          DefinitionName.plain("identity").toOption.get,
          TypeShape.Function(
            List(TypeShape.Identifier("Int")),
            TypeShape.Identifier("Int")
          ),
          shape
        )
        .isLeft
    )
  }

  private def completedDefinition: ConstructedDefinition =
    val binderId = BinderId(0)
    val body = ConstructedTerm
      .create(
        TermShape.Lambda1(
          binderId,
          "x",
          "Int",
          TermShape.BoundReference(binderId, "x")
        ),
        Vector(intType)
      )
      .toOption
      .get
    ConstructedDefinition
      .parameterlessDef(
        DefinitionName.plain("identity").toOption.get,
        functionType,
        body
      )
      .toOption
      .get

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef => Vector(value.tpt, value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.Select => Vector(value.qualifier)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.InfixOp => Vector(value.left, value.op, value.right)
      case value: untpd.PrefixOp => Vector(value.op, value.od)
      case value: untpd.Typed => Vector(value.expr, value.tpt)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Tuple => value.trees.toVector
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.If => Vector(value.cond, value.thenp, value.elsep)
      case value: untpd.Parens => Vector(value.t)
      case _ => Vector.empty
