package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import quasiquotes.definitions.DefinitionParameterClauseKind
import quasiquotes.neutral.ScalametaDefinitionProjection
import quasiquotes.parser.TermShape
import quasiquotes.terms.{ConstructedTerm, TermBinder}
import quasiquotes.terms.dotty.{ConstructedTermUntypedBackend, GeneratedOriginFragmentSupport}
import quasiquotes.types.TypeNormalForm
import scala.meta.Defn

/** NONPUBLIC test-local transaction. Preparation creates fragments, never an owner. */
private[dotty] object QuotedRootedAtomicOwnerModel:
  import ExistingClassUntypedRewrite.*
  import ExistingUntpdOrdinaryMethodTypeEditPreparation.*
  private val IntType = TypeNormalForm.STypeIdent("Int")
  final case class Edit(selection: QuotedUnaryProjection, replacement: Defn.Def)

  // These snapshots attest the retained graph, payload, sites and source bytes.
  // They are test-owned capabilities, not certificates usable by public plans.
  private final class Snapshot(root: untpd.Tree)(using Context):
    private val graph = ExistingUntpdClassMemberFilter.allTrees(root)
    private val sites = graph.map(t => (t.source,t.span,payload(t)))
    private val sources = graph.map(_.source).distinct.map(s => (s,new String(s.content)))
    def valid(using Context): Boolean =
      val current = ExistingUntpdClassMemberFilter.allTrees(root)
      current.size == graph.size && current.zip(graph).forall((a,b) => a.eq(b)) &&
        current.zip(sites).forall { case (t,(s,p,n)) => t.source.eq(s) && t.span == p && payload(t) == n } &&
        sources.forall((s,text) => new String(s.content) == text) &&
        current.forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice])

  private def payload(t: untpd.Tree): String = t match
    case i: untpd.Ident => i.name.toString
    case s: untpd.Select => s.name.toString
    case m: untpd.MemberDef => s"${m.name}:${m.mods.flags}"
    case l: untpd.Literal => l.const.toString
    case _ => ""

  private final class MethodFragments(
      val selection: QuotedUnaryProjection,
      val descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      val parameter: PreparedParameterType,
      val result: PreparedResultType,
      val body: untpd.Tree,
      val generated: SourceFile,
      val snapshot: Snapshot
  )

  final class Prepared private[QuotedRootedAtomicOwnerModel] (
      private val capture: Capture,
      private val original: Snapshot,
      private val fragments: Vector[MethodFragments]
  ):
    // Negative-test observation of fragments only: there is no staged owner.
    def bodyFragment(index: Int): untpd.Tree = fragments(index).body
    def generatedSource(index: Int): SourceFile = fragments(index).generated

    def emit()(using Context): Either[Failure, untpd.TypeDef] =
      for
        _ <- check(original.valid, "STALE_ORIGINAL", "original graph, site or source changed")
        _ <- ExistingUntpdClassMemberFilter.validateCaptured(capture.exact).left.map(p => failure(p.message))
        _ <- fragments.foldLeft[Either[Failure,Unit]](Right(())) { (acc,f) =>
          for
            _ <- acc
            _ <- validateSelection(capture,f.selection)
            _ <- ExistingUntpdOrdinaryMethodDescriptor.validate(f.descriptor).left.map(p => failure(p.message))
            _ <- validatePreparedParameterType(f.parameter,f.descriptor).left.map(p => failure(p.message))
            _ <- validatePreparedResultType(f.result,f.descriptor).left.map(p => failure(p.message))
            _ <- check(f.snapshot.valid, "STALE_FRAGMENT", "prepared fragment graph, source or span changed")
          yield ()
        }
      yield
        // Nothing above constructs a method, Template or TypeDef. Each selected
        // method is now copied once; the single owner is assembled only here.
        val replacements = fragments.map { f =>
          val old = f.descriptor.method
          val method = untpd.cpy.DefDef(old)(old.name,List(List(f.parameter.positionedParameter)),f.result.positionedType,f.body)
          f.selection.member.index -> method
        }.toMap
        val oldTemplate = capture.exact.originalTemplate
        val members = oldTemplate.body.zipWithIndex.map((m,i) => replacements.getOrElse(i,m))
        val template = untpd.cpy.Template(oldTemplate)(oldTemplate.constr,oldTemplate.parentsOrDerived,oldTemplate.derived,oldTemplate.self,members)
        val root = capture.exact.originalRoot
        untpd.cpy.TypeDef(root)(root.name,template)

  def prepare(capture: Capture, edits: Vector[Edit], omissions: Vector[MemberRef] = Vector.empty)(using Context): Either[Failure, Prepared] =
    for
      _ <- check(edits.size == 1 || edits.size == 2,"EDIT_COUNT","one or two edits required")
      _ <- check(omissions.isEmpty,"EDIT_CONFLICT","omission is outside this bounded transaction")
      _ <- check(edits.map(_.selection.member.index).distinct.size == edits.size,"EDIT_CONFLICT","duplicate selected member")
      _ <- ExistingUntpdClassMemberFilter.validateCaptured(capture.exact).left.map(p => failure(p.message))
      original = new Snapshot(capture.exact.originalRoot)
      _ <- check(original.valid,"ORIGINAL_PRE_TYPER","original must be clean pre-Typer")
      fragments <- edits.foldLeft[Either[Failure,Vector[MethodFragments]]](Right(Vector.empty)) { (acc,edit) =>
        for
          previous <- acc
          next <- prepareMethod(capture,edit)
        yield previous :+ next
      }
    yield new Prepared(capture,original,fragments)

  private def validateSelection(capture: Capture, selected: QuotedUnaryProjection)(using Context): Either[Failure,Unit] =
    for
      _ <- check(selected.capture.eq(capture),"FOREIGN_SELECTION","selection must belong to original Capture")
      descriptor <- ExistingUntpdOrdinaryMethodDescriptor.capture(capture.exact,selected.member.index).left.map(p => failure(p.message))
      _ <- check(selected.method.ref.methodIdentity.eq(descriptor.method) &&
        selected.method.ref.captureIdentity.eq(capture.exact) &&
        selected.method.ref.memberIndex == selected.member.index &&
        selected.parameter.ref.methodIdentity.eq(selected.method.ref) &&
        selected.parameter.ref.parameterIdentity.eq(descriptor.parameterClauses.head.head.tree) &&
        selected.parameter.ref.typeIdentity.eq(descriptor.parameterClauses.head.head.tpt) &&
        selected.method.body.identity.sameObjectAs(new ExactIdentity(descriptor.rhs)),
        "STALE_SELECTION","exact method, parameter and RHS handles required")
      // Reapply the original strict/hidden clause/body admission to current raw input.
      _ <- QuotedUnaryProjection.capture(capture,selected.method.diagnosticName)
      _ <- selected.method.parameterScope.reference(selected.parameter.ref)
    yield ()

  /** Only the output's OWN argument binder can map to this original scope. */
  def transport(selected: QuotedUnaryProjection, body: TermShape, own: TermBinder, parameter: ParameterRef)(using Context): Either[Failure,TermShape] =
    for
      // Structural TermShape equality does not carry a reference's side-table
      // provenance. Validate the exact handle before obtaining the scoped node.
      originalReference <- selected.method.parameterScope.reference(parameter)
      _ <- check(canonical(body),"ROOTED_GRAMMAR","only actual rooted Math/StrictMath abs paths are admitted")
      mapped <- CaptureLocalQuoteRewrite.mapQualifiedUnaryCall(body,own,originalReference)
    yield mapped

  private def canonical(body: TermShape): Boolean = body match
    case TermShape.Apply(TermShape.Select(TermShape.Select(TermShape.Select(TermShape.Select(
        TermShape.Identifier("_root_",false),"java"),"lang"),target),"abs"),List(_)) =>
      target == "Math" || target == "StrictMath"
    case _ => false

  private def prepareMethod(capture: Capture, edit: Edit)(using Context): Either[Failure,MethodFragments] =
    val selected = edit.selection
    for
      _ <- validateSelection(capture,selected)
      projected <- ScalametaDefinitionProjection.project(edit.replacement).left.map(p => failure(p.message))
      output <- projected.definition.asMethod.toRight(failure("method required"))
      _ <- check(projected.definition.name.source == selected.method.diagnosticName &&
        output.parameterClauses.size == 1 &&
        output.parameterClauses.head.kind == DefinitionParameterClauseKind.Ordinary &&
        output.parameterClauses.head.parameters.size == 1 &&
        output.parameterClauses.head.parameters.head.name.source == selected.parameter.diagnosticName &&
        output.parameterClauses.head.parameters.head.declaredType == IntType && output.resultType == IntType &&
        edit.replacement.tparams.isEmpty,
        "QUOTED_OUTPUT_SHAPE","matching non-generic strict unary Int output required")
      body <- output.body.toRight(failure("body required"))
      binder <- output.parameterScope.binder(0,0).left.map(p => failure(p.message))
      originalReference <- selected.method.parameterScope.reference(selected.parameter.ref)
      mapped <- transport(selected,body,binder,selected.parameter.ref)
      id <- originalReference match
        case TermShape.BoundReference(id,_) => Right(id)
        case _ => Left(failure("capture-local reference required"))
      constructed <- ConstructedTerm.fromShapeInScope(mapped,id).left.map(p => failure(p.toString))
      raw <- ConstructedTermUntypedBackend.lowerInScope(constructed,id,selected.parameter.diagnosticName).left.map(p => failure(p.toString))
      fragment <- GeneratedOriginFragmentSupport.planDefinitionBodyInScope(constructed,id,selected.parameter.diagnosticName).left.map(p => failure(p.toString))
      source = SourceFile.virtual("quoted-rooted-atomic.scala",fragment.source)
      positioned <- GeneratedOriginFragmentSupport.positionTerm(raw,fragment,source,0).left.map(p => failure(p.toString))
      _ <- GeneratedOriginFragmentSupport.validatePositionedTree(positioned,source,0,fragment.source.length).left.map(p => failure(p.toString))
      descriptor <- ExistingUntpdOrdinaryMethodDescriptor.capture(capture.exact,selected.member.index).left.map(p => failure(p.message))
      parameter <- prepareParameterType(descriptor,descriptor.parameterClauses.head.head,IntType).left.map(p => failure(p.message))
      result <- prepareResultType(descriptor,IntType).left.map(p => failure(p.message))
      atSite = positioned.cloneIn(descriptor.rhs.source).withSpan(descriptor.rhs.span)
      snapshot = new Snapshot(atSite)
      _ <- check(snapshot.valid,"BODY_PRE_TYPER","generated body must be clean pre-Typer")
    yield new MethodFragments(selected,descriptor,parameter,result,atSite,source,snapshot)

  private def check(value: Boolean,code: String,detail: String): Either[Failure,Unit] =
    Either.cond(value,(),Failure(code,detail))
  private def failure(detail: String): Failure = Failure("PRIVATE_ATOMIC_PREPARATION",detail)
