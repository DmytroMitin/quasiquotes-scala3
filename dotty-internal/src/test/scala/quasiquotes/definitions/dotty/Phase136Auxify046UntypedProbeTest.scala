package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

class Phase136Auxify046UntypedProbeTest extends munit.FunSuite:
  private val Canonical =
    "type Self >: self.type <: Nat { type Self = self.Self }"
  private val Renamed =
    "type Element >: owner$2.type <: Domain { type Element = owner$2.Element }"

  test("the parser exposes the exact canonical nine-node TypeDef tree and spans") {
    withContext {
      parseOne(Canonical) { parsed =>
        assertExactShape(parsed, "Self", "self", "Nat")
        assertEquals(structure(parsed), expectedStructure("Self", "self", "Nat"))
        assertEquals(
          nonEmptyTrees(parsed).map(tree =>
            (tree.getClass.getSimpleName, tree.span.start, tree.span.point, tree.span.end)
          ),
          Vector(
            ("TypeDef", 0, 5, 55),
            ("TypeBoundsTree", 10, 10, 55),
            ("SingletonTypeTree", 13, 13, 22),
            ("Ident", 13, 13, 17),
            ("RefinedTypeTree", 26, 26, 55),
            ("Ident", 26, 26, 29),
            ("TypeDef", 32, 37, 53),
            ("Select", 44, 49, 53),
            ("Ident", 44, 44, 48)
          )
        )
        assert(!parsed.source.exists)
      }
    }
  }

  test("a source-free constructor reproduces canonical and renamed parser structures") {
    withContext {
      List(
        (Canonical, "Self", "self", "Nat"),
        (Renamed, "Element", "owner$2", "Domain")
      ).foreach { case (source, member, selfAlias, upperBase) =>
        val parsedStructure = parseOne(source)(structure)
        val constructed = constructEquivalent(member, selfAlias, upperBase)

        assertExactShape(constructed, member, selfAlias, upperBase)
        assertEquals(structure(constructed), parsedStructure)
        val nodes = nonEmptyTrees(constructed)
        assertEquals(nodes.size, 9)
        nodes.foreach { tree =>
          assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
          assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
          assertEquals(tree.symbol, NoSymbol, clues(tree.getClass.getSimpleName))
          assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree.getClass.getSimpleName))
        }
      }
    }
  }

  test("deterministic generated source can recursively position all nine nodes") {
    withContext {
      List(("Self", "self", "Nat"), ("Element", "owner$2", "Domain")).foreach {
        case (member, selfAlias, upperBase) =>
          val raw = constructEquivalent(member, selfAlias, upperBase)
          val generated =
            s"type $member >: $selfAlias.type <: $upperBase { type $member = $selfAlias.$member }"
          val source = SourceFile.virtual("Phase136Auxify046Generated.scala", generated)
          val positioned = positionEquivalent(raw, member, selfAlias, upperBase, source)

          assertExactShape(positioned, member, selfAlias, upperBase)
          assertEquals(structure(positioned), structure(raw))
          val nodes = nonEmptyTrees(positioned)
          assertEquals(nodes.size, 9)
          nodes.foreach { tree =>
            assert(tree.source.exists, clues(tree.getClass.getSimpleName))
            assertEquals(tree.source.path, source.path)
            assertEquals(tree.source.content.mkString, generated)
            assert(tree.span.exists, clues(tree.getClass.getSimpleName))
            assert(tree.span.start >= 0)
            assert(tree.span.start <= tree.span.point)
            assert(tree.span.point <= tree.span.end)
            assert(tree.span.end <= generated.length)
            assertEquals(tree.symbol, NoSymbol)
            assert(!tree.isInstanceOf[untpd.TypedSplice])
            directChildren(tree).foreach { child =>
              assert(child.span.start >= tree.span.start)
              assert(child.span.end <= tree.span.end)
            }
          }
          nodes.foreach { tree =>
            directChildren(tree).zip(directChildren(tree).drop(1)).foreach {
              case (left, right) => assert(left.span.end <= right.span.start)
            }
          }
      }
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parseOne[A](source: String)(
      run: untpd.TypeDef => A
  )(using outerContext: Context): A =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parser(SourceFile.virtual("Phase136Auxify046Probe.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    val definition = parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head match
          case value: untpd.TypeDef => value
          case other => fail(s"expected TypeDef, found ${other.getClass.getSimpleName}")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")
    run(definition)

  private def constructEquivalent(
      member: String,
      selfAlias: String,
      upperBase: String
  )(using Context): untpd.TypeDef =
    given SourceFile = NoSource
    val lower =
      untpd.SingletonTypeTree(untpd.Ident(termName(selfAlias)))
    val selected =
      untpd.Select(untpd.Ident(termName(selfAlias)), typeName(member))
    val refinementMember = untpd.TypeDef(typeName(member), selected)
    val upper =
      untpd.RefinedTypeTree(
        untpd.Ident(typeName(upperBase)),
        refinementMember :: Nil
      )
    untpd.TypeDef(typeName(member), untpd.TypeBoundsTree(lower, upper))

  private def positionEquivalent(
      raw: untpd.TypeDef,
      member: String,
      selfAlias: String,
      upperBase: String,
      source: SourceFile
  )(using Context): untpd.TypeDef =
    val rendered = source.content.mkString
    val outerPoint = "type ".length
    val lowerStart = rendered.indexOf(selfAlias, outerPoint + member.length)
    val lowerEnd = lowerStart + selfAlias.length + ".type".length
    val baseStart = rendered.indexOf(upperBase, lowerEnd)
    val baseEnd = baseStart + upperBase.length
    val aliasStart = rendered.indexOf("type ", baseEnd)
    val aliasPoint = aliasStart + "type ".length
    val selectedStart = rendered.indexOf(selfAlias, aliasPoint + member.length)
    val selectedPoint = selectedStart + selfAlias.length + 1
    val selectedEnd = selectedPoint + member.length

    raw.rhs match
      case untpd.TypeBoundsTree(
            lower: untpd.SingletonTypeTree,
            upper: untpd.RefinedTypeTree,
            alias
          ) if alias.isEmpty =>
        val positionedLower = lower.ref match
          case identifier: untpd.Ident =>
            untpd
              .SingletonTypeTree(
                identifier
                  .cloneIn(source)
                  .withSpan(Span(lowerStart, lowerStart + selfAlias.length, lowerStart))
              )
              .cloneIn(source)
              .withSpan(Span(lowerStart, lowerEnd, lowerStart))
          case other => fail(s"expected singleton identifier, found $other")
        val positionedUpper = upper match
          case untpd.RefinedTypeTree(base: untpd.Ident, List(alias: untpd.TypeDef)) =>
            val positionedBase =
              base.cloneIn(source).withSpan(Span(baseStart, baseEnd, baseStart))
            val positionedAlias = alias.rhs match
              case selected: untpd.Select =>
                val positionedPrefix = selected.qualifier match
                  case prefix: untpd.Ident =>
                    prefix
                      .cloneIn(source)
                      .withSpan(
                        Span(selectedStart, selectedStart + selfAlias.length, selectedStart)
                      )
                  case other => fail(s"expected selected identifier, found $other")
                val positionedSelected =
                  untpd
                    .Select(positionedPrefix, selected.name)
                    .cloneIn(source)
                    .withSpan(Span(selectedStart, selectedEnd, selectedPoint))
                untpd
                  .TypeDef(alias.name, positionedSelected)
                  .withMods(alias.mods)
                  .cloneIn(source)
                  .withSpan(Span(aliasStart, selectedEnd, aliasPoint))
              case other => fail(s"expected selected alias RHS, found $other")
            untpd
              .RefinedTypeTree(positionedBase, positionedAlias :: Nil)
              .cloneIn(source)
              .withSpan(Span(baseStart, rendered.length, baseStart))
          case other => fail(s"expected refined upper bound, found $other")
        val positionedBounds =
          untpd
            .TypeBoundsTree(positionedLower, positionedUpper)
            .cloneIn(source)
            .withSpan(Span(lowerStart, rendered.length, lowerStart))
        untpd
          .TypeDef(raw.name, positionedBounds)
          .withMods(raw.mods)
          .cloneIn(source)
          .withSpan(Span(0, rendered.length, outerPoint))
      case other => fail(s"expected raw TypeBoundsTree, found $other")

  private def assertExactShape(
      definition: untpd.TypeDef,
      member: String,
      selfAlias: String,
      upperBase: String
  ): Unit =
    assertEquals(definition.name.toString, member)
    assert(!definition.mods.hasFlags)
    definition.rhs match
      case untpd.TypeBoundsTree(
            untpd.SingletonTypeTree(untpd.Ident(lowerAlias)),
            untpd.RefinedTypeTree(
              untpd.Ident(base),
              List(refinementMember: untpd.TypeDef)
            ),
            alias
          ) =>
        assert(alias.isEmpty)
        assertEquals(lowerAlias.toString, selfAlias)
        assertEquals(base.toString, upperBase)
        assertEquals(refinementMember.name.toString, member)
        assert(!refinementMember.mods.hasFlags)
        refinementMember.rhs match
          case untpd.Select(untpd.Ident(prefix), selected) =>
            assertEquals(prefix.toString, selfAlias)
            assertEquals(selected.toString, member)
          case other => fail(s"expected direct selected-Type alias RHS, found $other")
      case other => fail(s"expected exact two-bound refined TypeDef, found $other")

  private def expectedStructure(member: String, selfAlias: String, base: String): String =
    s"TypeDef($member,0,TypeBounds(Singleton(Ident($selfAlias)),Refined(Ident($base),List(TypeDef($member,0,Select(Ident($selfAlias),$member)))),Empty))"

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.SingletonTypeTree =>
        s"Singleton(${structure(value.ref)})"
      case value: untpd.RefinedTypeTree =>
        s"Refined(${structure(value.tpt)},${value.refinements.map(structure)})"
      case value: untpd.Select =>
        s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def nonEmptyTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.SingletonTypeTree => Vector(value.ref)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty
