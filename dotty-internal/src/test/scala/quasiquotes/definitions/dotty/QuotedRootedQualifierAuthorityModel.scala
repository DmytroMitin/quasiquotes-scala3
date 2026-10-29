package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import quasiquotes.neutral.ScalametaDefinitionProjection
import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm
import quasiquotes.terms.dotty.{ConstructedTermUntypedBackend, GeneratedOriginFragmentSupport}
import scala.meta.*

/** NONPUBLIC test oracle. No public rewrite accepts this rooted RHS. */
private[dotty] object QuotedRootedQualifierAuthorityModel:
  import ExistingClassUntypedRewrite.*

  def quote(selected: QuotedUnaryProjection): Either[Failure, Defn.Def] = selected.tree match
    case q"def echo($value: AnyVal): AnyVal = $body" => value match
      case term: Term.Name => Right(q"def echo($term: Int): Int = _root_.java.lang.Math.abs($term)")
      case _ => Left(Failure("QUOTE_NAME_NARROWING", "expected Term.Name"))
    case q"def magnitude($number: AnyVal): AnyVal = $body" => number match
      case term: Term.Name => Right(q"def magnitude($term: Int): Int = _root_.java.lang.StrictMath.abs($term)")
      case _ => Left(Failure("QUOTE_NAME_NARROWING", "expected Term.Name"))
    case _ => Left(Failure("QUOTE_PATTERN_MISMATCH", "expected real echo or magnitude"))

  def mapped(selected: QuotedUnaryProjection, replacement: Defn.Def)(using Context): Either[Failure, TermShape] =
    for
      projected <- ScalametaDefinitionProjection.project(replacement).left.map(p => Failure("PROJECTION", p.message))
      output <- projected.definition.asMethod.toRight(Failure("METHOD_REQUIRED", "method required"))
      binder <- output.parameterScope.binder(0, 0).left.map(p => Failure("BINDER", p.message))
      ref <- selected.method.parameterScope.reference(selected.parameter.ref)
      shape <- CaptureLocalQuoteRewrite.mapQualifiedUnaryCall(output.body.get, binder, ref)
    yield shape

  // Absence of a direct member does not prove absence in imports, parents,
  // enclosing owners or block scopes. No declaration certificate is issued.
  def canonicalContextCheck(selected: QuotedUnaryProjection, qualifier: String)(using Context): Either[Failure, Unit] =
    val root = selected.capture.exact.originalRoot
    val direct = root.rhs.asInstanceOf[untpd.Template].body.exists {
      case m: untpd.MemberDef => m.name.toString == qualifier
      case _ => false
    }
    Left(Failure(if direct then "DIRECT_QUALIFIER_SHADOW" else "ORIGINAL_CONTEXT_QUALIFIER_UNPROVEN",
      "raw member spellings can reject a collision but cannot certify a free qualifier declaration"))

  final class Fragment private[QuotedRootedQualifierAuthorityModel] (
      val selection: QuotedUnaryProjection,
      val owner: untpd.TypeDef,
      val method: untpd.DefDef,
      val source: SourceFile,
      val tree: untpd.Tree,
      private[QuotedRootedQualifierAuthorityModel] val graph: Vector[untpd.Tree]
  ):
    private[QuotedRootedQualifierAuthorityModel] val sites = graph.map(t => (t.source,t.span))
    private[QuotedRootedQualifierAuthorityModel] val content = new String(source.content)

  def prepare(selected: QuotedUnaryProjection)(using Context): Either[Failure, Fragment] =
    def err(value: Any) = Failure("PRIVATE_ROOTED_PREPARATION", value.toString)
    for
      replacement <- quote(selected)
      shape <- mapped(selected, replacement)
      ref <- selected.method.parameterScope.reference(selected.parameter.ref)
      id <- ref match
        case TermShape.BoundReference(id, _) => Right(id)
        case _ => Left(err("original capture-local reference required"))
      constructed <- ConstructedTerm.fromShapeInScope(shape, id).left.map(err)
      raw <- ConstructedTermUntypedBackend.lowerInScope(constructed, id, selected.parameter.diagnosticName).left.map(err)
      fragment <- GeneratedOriginFragmentSupport.planDefinitionBodyInScope(constructed, id, selected.parameter.diagnosticName).left.map(err)
      source = SourceFile.virtual("quoted-rooted-call.scala", fragment.source)
      positioned <- GeneratedOriginFragmentSupport.positionTerm(raw, fragment, source, 0).left.map(err)
      _ <- GeneratedOriginFragmentSupport.validatePositionedTree(positioned, source, 0, fragment.source.length).left.map(err)
      method = selected.capture.exact.members(selected.member.index).tree.asInstanceOf[untpd.DefDef]
      // The outer RHS keeps the transformation site; all six descendants keep
      // exact generated positions on the freshly allocated source handle.
      tree = positioned.cloneIn(method.rhs.source).withSpan(method.rhs.span)
    yield new Fragment(selected, selected.capture.exact.originalRoot, method, source, tree, nodes(tree))

  def validate(selected: QuotedUnaryProjection, fragment: Fragment, tree: untpd.Tree)(using Context): Either[Failure, Unit] =
    val current = nodes(tree)
    Either.cond(
      fragment.selection.eq(selected) &&
        fragment.owner.eq(selected.capture.exact.originalRoot) &&
        fragment.method.eq(selected.capture.exact.members(selected.member.index).tree) &&
        tree.eq(fragment.tree) && current.size == 7 &&
        current.size == fragment.graph.size && current.zip(fragment.graph).forall((a,b) => a.eq(b)) &&
        current.zip(fragment.sites).forall { case (t,(source,span)) => t.source.eq(source) && t.span == span } &&
        new String(fragment.source.content) == fragment.content &&
        tree.source.eq(fragment.method.rhs.source) && tree.span == fragment.method.rhs.span &&
        current.tail.forall(t => t.source.eq(fragment.source) && t.span.exists) &&
        current.forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice]),
      (), Failure("PRIVATE_ROOTED_EVIDENCE_MISMATCH", "owner, method, exact fresh graph or source handle mismatch"))

  def rewrite(selected: QuotedUnaryProjection, fragment: Fragment)(using Context): Either[Failure, untpd.TypeDef] =
    for
      _ <- validate(selected, fragment, fragment.tree)
      // Only existing public Type operations are used; the rooted body cannot
      // pass public apply and is installed by this NONPUBLIC oracle afterwards.
      p1 <- selected.capture.emptyPlan.replaceParameterType(selected.parameter.ref, _root_.quasiquotes.types.TypeNormalForm.STypeIdent("Int"))
      p2 <- p1.replaceResultType(selected.method.ref, _root_.quasiquotes.types.TypeNormalForm.STypeIdent("Int"))
      typed <- ExistingClassUntypedRewrite(selected.capture, p2)
      template = typed.tree.rhs.asInstanceOf[untpd.Template]
      method = template.body(selected.member.index).asInstanceOf[untpd.DefDef]
      changed = untpd.cpy.DefDef(method)(method.name, method.paramss, method.tpt, fragment.tree)
      freshTemplate = untpd.cpy.Template(template)(template.constr, template.parentsOrDerived, template.derived, template.self, template.body.updated(selected.member.index, changed))
      root = untpd.cpy.TypeDef(typed.tree)(typed.tree.name, freshTemplate)
    yield root

  def nodes(tree: untpd.Tree): Vector[untpd.Tree] = tree match
    case a: untpd.Apply => Vector(tree) ++ nodes(a.fun) ++ a.args.toVector.flatMap(nodes)
    case s: untpd.Select => Vector(tree) ++ nodes(s.qualifier)
    case _ => Vector(tree)
