package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import quasiquotes.parser.TermShape

/** Private immutable graph attestation, shared by selection and rooted preparation. */
private[dotty] object ExistingQuotedRootedBody:
  def canonical(shape: TermShape): Boolean = shape match
    case TermShape.Apply(TermShape.Select(TermShape.Select(TermShape.Select(TermShape.Select(
        TermShape.Identifier("_root_",false),"java"),"lang"),target),"abs"),List(_: TermShape.BoundReference)) =>
      target == "Math" || target == "StrictMath"
    case _ => false

  def nodes(tree: untpd.Tree): Vector[untpd.Tree] = tree match
    case a: untpd.Apply => Vector(tree) ++ nodes(a.fun) ++ a.args.toVector.flatMap(nodes)
    case s: untpd.Select => Vector(tree) ++ nodes(s.qualifier)
    case _ => Vector(tree)

  /** Ordinary compiler traversers omit modifiers and their annotation trees. */
  def originalGraph(root: untpd.Tree): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    def visit(value: Any): Unit = value match
      case null => throw new IllegalArgumentException("unavailable original graph field")
      case _: dotty.tools.dotc.ast.Trees.Lazy[?] =>
        throw new IllegalArgumentException("unforced original graph field")
      case tree: untpd.Tree =>
        builder += tree
        if !tree.isEmpty then
          tree.productIterator.foreach(visit)
          tree match
            case function: untpd.FunctionWithMods =>
              visit(function.mods)
              visit(function.erasedParams)
            case definition: untpd.DefTree => visit(definition.mods)
            case _ => ()
      case modifiers: untpd.Modifiers => modifiers.productIterator.foreach(visit)
      case values: Iterable[?] => values.foreach(visit)
      case _ => ()
    visit(root)
    builder.result()

  private def modifiersOf(tree: untpd.Tree): Option[untpd.Modifiers] = tree match
    case function: untpd.FunctionWithMods => Some(function.mods)
    case definition: untpd.DefTree => Some(definition.mods)
    case _ => None

  final class Snapshot(graph: Vector[untpd.Tree]):
    private val sites = graph.map(t => (t.source,t.span,payload(t)))
    private val modifiers = graph.flatMap { tree =>
      modifiersOf(tree).map(value =>
        (tree,value,value.flags,value.privateWithin,value.annotations,value.mods))
    }
    private val modifierSites = modifiers.flatMap { case (_,value,_,_,_,_) =>
      value.mods.map(mod => (mod,mod.source,mod.span))
    }
    // SourceFile.equals compares file/start, not the identity of its mutable
    // content buffer. Distinct equal-valued handles must each be snapshotted.
    private val sources = (graph.map(_.source) ++ modifierSites.map(_._2))
      .foldLeft(Vector.empty[dotty.tools.dotc.util.SourceFile]) { (seen,source) =>
        if seen.exists(_.eq(source)) then seen else seen :+ source
      }.map(source => (source,new String(source.content)))

    def valid(current: => Vector[untpd.Tree])(using Context): Boolean =
      try
        val latest = current
        latest.size == graph.size && latest.zip(graph).forall((a,b) => a.eq(b)) &&
          latest.zip(sites).forall { case (t,(s,p,n)) => t.source.eq(s) && t.span == p && payload(t) == n } &&
          modifiers.forall { case (tree,original,flags,within,annotations,syntax) =>
            modifiersOf(tree).exists(value => value.eq(original) && value.flags == flags &&
              value.privateWithin == within && value.annotations.eq(annotations) && value.mods.eq(syntax))
          } &&
          modifierSites.forall((mod,source,span) => mod.source.eq(source) && mod.span == span) &&
          sources.forall((s,text) => new String(s.content) == text) &&
          latest.forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice])
      catch case scala.util.control.NonFatal(_) => false

  private def payload(tree: untpd.Tree): String = tree match
    case i: untpd.Ident => i.name.toString
    case s: untpd.Select => s.name.toString
    case m: untpd.MemberDef => s"${m.name}:${m.mods.flags}"
    case l: untpd.Literal => l.const.toString
    case n: untpd.Number => s"${n.digits}:${n.kind}"
    case _ => ""
