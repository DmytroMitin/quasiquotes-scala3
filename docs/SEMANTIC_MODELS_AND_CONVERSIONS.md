# Semantic models and conversions

Quasiquotes uses three representation worlds. They overlap, but they are not
interchangeable and whitespace is not the distinction between them.

| World | Purpose and breadth | Fidelity, identity, and resolution | Best use |
| --- | --- | --- | --- |
| Scalameta source AST (`scala.meta`) | Broad Scala source grammar for parsing, source-like construction, and matching. | Parsed trees can retain tokens, comments, positions, and source spelling. Freshly constructed trees normally have `Position.None`. Scalameta structure represents lexical names but does not by itself perform compiler name/type/member resolution. | Source tooling and fresh source-like authoring. |
| Project-owned compiler-free semantic model | Deliberately bounded, validated Term, Type, and Definition meanings shared by frontends and backends. | It may normalize or forget source distinctions. `TypeNormalForm` normalizes Type structure; Term and Definition binding uses explicit opaque graph-local co-reference and alpha-aware semantics. It has no compiler symbols or ambient resolution. Resolved Type identities can carry compiler-free owner segments; a bounded explicit environment is needed where an external source representation cannot recover their owner kinds. | Portable semantic inspection, construction, matching, and composition inside admitted families. |
| Exact Dotty `untpd` AST | Exact pre-Typer compiler topology for one full Scala compiler version. | Raw node kinds, child topology, `SourceFile`, spans, and sometimes object identity are contractual. Fresh nodes normally have `NoSymbol`; resolution and typing require the compiler lifecycle and a matching `Context`. | Exact fresh raw-tree construction and identity-sensitive transformation of existing raw trees. |

The project model is not a lossless copy of Scalameta, and Scalameta is not a
portable spelling of an existing Dotty graph. Dotty raw constructors are exact
but are not an ergonomic general-purpose authoring language.

## Programme vocabulary

The architecture labels mean:

- **Q** — the Quotes-aware typed quasiquote/frontend world. It operates in the
  caller's active `Quotes` universe and is the direction most directly exposed
  by public `qr`/`qq`, `tqr`/`tqq`, and `dqr`/`dqq` syntax.
- **N** — the neutral/compiler-free project semantic model plus bounded
  Scalameta Projection and fresh Authoring interoperability.
- **U-D**, **exact fresh lowering** — fresh exact Dotty `untpd` construction
  from a validated project semantic value or bounded plan.
- **U-U**, **exact existing-tree transformation** — capture, preservation,
  rewrite, origin adaptation, and reconstruction of an existing exact `untpd`
  graph.
- **C** — cross-layer composition, integration, API policy, peer delivery, and
  controller ownership. C is not another AST.

These invariants avoid misleading syntax in diagrams:

```text
N != public n* syntax
U != public u* syntax
C != another tree universe
public u* syntax is not currently selected; it remains a later optional layer
```

## Current conversion graph

The compiler-free public Term and Type pairs are current:

```text
scala.meta.Term
  -> public ScalametaTermProjection
  -> public ProjectedTermShape / TermShape
  -> public ScalametaTermShapeAuthoring
  -> fresh scala.meta.Term

scala.meta.Type (current unresolved recursive subset only)
  -> public ScalametaTypeNormalFormProjection
  -> public ProjectedTypeNormalForm / TypeNormalForm
  -> public ScalametaTypeNormalFormAuthoring
  -> fresh scala.meta.Type (unresolved recursive subset; resolved identities rejected)

scala.meta.Defn
  -> public ScalametaDefinitionProjection
  -> public ProjectedDefinition / SemanticDefinition
  -> public ScalametaDefinitionAuthoring
  -> fresh scala.meta.Defn
```

Term Projection and Authoring currently overlap across literals, identifiers,
selections, one-list Apply, infix, unary, tuples, explicit `if`, standard `s`
interpolation, primitive ascription, the fixed one-list constructor family,
typed Lambda1, binder-free P1, one typed local immutable value (P2), and one
source-owned local identity method (P3). Projection erases grouping parentheses
because Scalameta 4.17.3 has no distinct structural parenthesized `Term`; a
fresh parenthesized Term therefore cannot be reconstructed from the semantic
value. Public `TermShapeBindings` and `TermShapeBindingView` make the
binder-bearing overlap safe without exposing private cases or raw binder IDs.

`SemanticDefinition` is the current public Core model. It supports validated
smart construction and typed views for bounded immutable values,
zero/one/two-ordinary-parameter concrete methods, and simple aliases. The
public `ScalametaDefinitionProjection.project(Defn)` facade returns a
`ProjectedDefinition` containing that semantic value and an optional truthful
root `NeutralSourceSpan`. The public
`ScalametaDefinitionAuthoring.author(SemanticDefinition)` facade constructs a
fresh `Defn` with `Position.None` and checks semantic reprojection. These are
semantic conversions, not token, comment, formatting, or position inverses.

The private five-family `DefinitionShape` carrier and its dispatchers remain
implementation details. Specialized contextual, instance, delegated,
self-member, and refined-AUX projectors/authorers/bridges remain
separate bounded contracts rather than generic fallbacks. In particular, the
accepted package-private instance-factory reverse edge authors one fresh
Scalameta `Defn.Def` from its five-role semantic plan and verifies
alpha-equivalent reprojection; it is not a public generic
`SemanticDefinition` authorer.

The public exact-version conveniences currently accept Scalameta inputs:

```text
scala.meta.Term -> public ScalametaTermUntypedBridge -> fresh source-free untpd.Tree
scala.meta.Type -> public ScalametaTypeUntypedBridge -> fresh source-free untpd.Tree
scala.meta.Defn -> public ScalametaDefinitionUntypedBridge -> fresh source-free untpd.MemberDef
```

The project-semantic source-free facades are also current:

```text
TermShape -> public TermUntypedLowering -> fresh source-free untpd.Tree
TypeNormalForm -> public TypeUntypedLowering -> fresh source-free untpd.Tree
SemanticDefinition -> public DefinitionUntypedLowering -> fresh source-free untpd.MemberDef
```

The Term facade uses the richer completed-Term path and therefore admits the
current binder-safe Lambda/local-value/local-method semantic families. The
older Scalameta Term bridge remains a narrower non-delegating compatibility
surface. The context-free Scalameta Type bridge delegates through
`TypeUntypedLowering`. The Scalameta Definition bridge remains a separate
non-delegating five-family composition. All successful results are fresh and
source-free; none claims generated origin, owner assignment, placement,
typechecking, retyping, or existing-tree rewriting.

Accepted package-private U-U mechanisms separately transform selected existing
raw trees. Current examples include atomic single-parameter method rewrites and
a two-parameter method RHS-only rewrite with exact preservation of non-target
children. No general public exact-U capture/rewrite API exists yet.

## Loss and provenance model

Use this model for every conversion:

```text
Scalameta -> project Projection may normalize or forget source distinctions.
Project -> Scalameta Authoring creates fresh syntax; it does not recover original formatting, tokens, comments, or positions.
Project -> untpd exact fresh lowering constructs new raw syntax only for an admitted semantic family.
Existing untpd -> exact U transformation preserves exact objects where promised and must not launder the whole owner through Scalameta.
```

A projected Scalameta root may carry truthful start/end offsets as
`NeutralSourceSpan`. That span is metadata beside the semantic value; it is not
recursively reconstructed by Authoring. Fresh Scalameta syntax uses
`Position.None`. Source-free exact lowering creates raw trees with no source or
span. Generated-origin bridges instead create a deterministic virtual
`SourceFile` and recursively contained truthful spans for the newly generated
graph. Existing-tree transformation has its own identity and provenance rule:
preserved nodes may remain the exact original objects while fresh replacement
nodes receive only a truthful transformation or generated site.

## Checked Term and Type hello world

This external-package source is compiled in `neutralScalameta/test`. The
documentation guard compares the block byte for byte with the checked source.
The assertions use semantic reprojection rather than original source bytes.

<!-- snippet:semantic-term-type:start -->
```scala
import quasiquotes.definitions.*
import quasiquotes.neutral.*
import quasiquotes.terms.*
import quasiquotes.types.TypeNormalForm

import scala.meta.*
import scala.meta.dialects.Scala3

object SemanticTermTypeHelloWorld:
  def check(): Unit =
    val projectedTerm = ScalametaTermProjection.project(q"1 + 2")
    val termShape = projectedTerm.fold(error => sys.error(error.message), _.shape)
    val authoredTerm = ScalametaTermShapeAuthoring
      .author(termShape)
      .fold(error => sys.error(error.message), identity)
    val reprojectedTerm = ScalametaTermProjection
      .project(authoredTerm)
      .fold(error => sys.error(error.message), _.shape)

    assert(reprojectedTerm == termShape)
    assert(authoredTerm.pos == Position.None)

    val intType = TypeNormalForm.STypeIdent("Int")
    val lambda = TermShapeBindings
      .lambda(Vector(TermParameterSpec("x", intType))) { scope =>
        scope.reference(scope.parameterBinders.head.head)
      }
      .fold(error => sys.error(error.message), identity)
    val lambdaView = TermShapeBindingView
      .inspect(lambda)
      .fold(error => sys.error(error.message), identity)
      .lambda
      .get
    val bodyBinder = TermShapeBindingView
      .inspect(lambdaView.body)
      .fold(error => sys.error(error.message), identity)
      .boundReference
      .get
      .binder

    assert(bodyBinder == lambdaView.parameters.head.binder)

    val projectedType = ScalametaTypeNormalFormProjection.project(t"List[Int]")
    val normalForm = projectedType.fold(error => sys.error(error.message), _.normalForm)
    val authoredType = ScalametaTypeNormalFormAuthoring
      .author(normalForm)
      .fold(error => sys.error(error.message), identity)
    val reprojectedType = ScalametaTypeNormalFormProjection
      .project(authoredType)
      .fold(error => sys.error(error.message), _.normalForm)

    assert(reprojectedType == normalForm)
    assert(authoredType.pos == Position.None)

    val definitions = Vector(
      q"val foo: Int = 42".asInstanceOf[Defn],
      q"def foo(x: Int): String = x.toString".asInstanceOf[Defn],
      q"type T = Int".asInstanceOf[Defn]
    )

    definitions.foreach { source =>
      val projected = ScalametaDefinitionProjection
        .project(source)
        .fold(error => sys.error(error.message), identity)
      val authored = ScalametaDefinitionAuthoring
        .author(projected.definition)
        .fold(error => sys.error(error.message), identity)
      val reprojected = ScalametaDefinitionProjection
        .project(authored)
        .fold(error => sys.error(error.message), identity)

      assert(reprojected.definition == projected.definition)
      assert(reprojected.sourceSpan.isEmpty)
      assert(authored.pos == Position.None)
    }
```
<!-- snippet:semantic-term-type:end -->

## Checked public `SemanticDefinition` hello world

This example is source-independent Core construction. It does not claim that
the values came from Scalameta. The method body uses the public Term semantic
route to express the admitted `x.toString` selection, and the persistent method
scope proves binder co-reference without spelling-based inference.

<!-- snippet:semantic-definition-core:start -->
```scala
import quasiquotes.definitions.*
import quasiquotes.parser.TermShape
import quasiquotes.terms.TermShapeBindingView
import quasiquotes.types.TypeNormalForm

object SemanticDefinitionCoreHelloWorld:
  private val intType = TypeNormalForm.STypeIdent("Int")
  private val stringType = TypeNormalForm.STypeIdent("String")

  private def name(source: String): DefinitionName =
    DefinitionName.fromSource(source).fold(error => sys.error(error.message), identity)

  def check(): Unit =
    val value = SemanticDefinition
      .immutableValue(name("foo"), intType, TermShape.Literal("42"))
      .fold(error => sys.error(error.message), identity)

    val parameter = DefinitionParameter(name("x"), intType)
    val ordinaryClause = DefinitionParameterClause
      .ordinary(Vector(parameter))
      .fold(error => sys.error(error.message), identity)
    val method = SemanticDefinition
      .concreteMethod(name("foo"), Vector(ordinaryClause), stringType) { scope =>
        scope.reference(0, 0).map(reference => TermShape.Select(reference, "toString"))
      }
      .fold(error => sys.error(error.message), identity)

    val alias = SemanticDefinition
      .typeAlias(name("T"), intType)
      .fold(error => sys.error(error.message), identity)

    assert(value.asValue.get.body.contains(TermShape.Literal("42")))
    assert(alias.asType.get.aliasedType.contains(intType))

    val methodView = method.asMethod.get
    val parameterBinder = methodView.parameterScope
      .binder(0, 0)
      .fold(error => sys.error(error.message), identity)
    val bodyQualifier = methodView.body.get.asInstanceOf[TermShape.Select].qualifier
    val bodyBinder = TermShapeBindingView
      .inspect(bodyQualifier)
      .fold(error => sys.error(error.message), identity)
      .boundReference
      .get
      .binder

    assert(parameterBinder == bodyBinder)
```
<!-- snippet:semantic-definition-core:end -->

## Checked source-free semantic lowerers and Scalameta bridges

These APIs expose Dotty internals and therefore require the exact matching
compiler artifact. Term and Definition lowering also require an active Dotty
`Context`; the test fixture uses `ContextBase.initialCtx` only to demonstrate
the raw source-free result. A real compiler plugin should use its active phase
context. The Type bridge is context-free. These fresh raw results have no
source, span, owner, or symbol. In particular, a source-free `untpd.InfixOp`
is structurally correct but is not automatically typable: the tested Dotty
infix desugaring reads spans.

<!-- snippet:dotty-source-free:start -->
```scala
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

import quasiquotes.definitions.dotty.{DefinitionUntypedLowering, ScalametaDefinitionUntypedBridge}
import quasiquotes.neutral.*
import quasiquotes.terms.dotty.{ScalametaTermUntypedBridge, TermUntypedLowering}
import quasiquotes.types.dotty.{ScalametaTypeUntypedBridge, TypeUntypedLowering}

import scala.meta.*
import scala.meta.dialects.Scala3

object DottySourceFreeHelloWorld:
  def check(): Unit = withContext:
    val sourceTerm = q"1 + 2"
    val termShape = ScalametaTermProjection
      .project(sourceTerm)
      .fold(error => sys.error(error.message), _.shape)
    val term = TermUntypedLowering
      .lower(termShape)
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val bridgedTerm = ScalametaTermUntypedBridge
      .lower(sourceTerm)
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    val sourceType = t"List[Int]"
    val normalForm = ScalametaTypeNormalFormProjection
      .project(sourceType)
      .fold(error => sys.error(error.message), _.normalForm)
    val loweredType = TypeUntypedLowering
      .lower(normalForm)
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val bridgedType = ScalametaTypeUntypedBridge
      .lower(sourceType)
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    val sourceDefinitions = Vector(
      "val foo: Int = 42",
      "def foo(x: Int): String = x.toString",
      "type T = Int"
    ).map(parseDefinition)
    val semanticDefinitions = sourceDefinitions.map { source =>
      ScalametaDefinitionProjection
        .project(source)
        .fold(error => sys.error(error.message), _.definition)
    }
    val authoredDefinitions = semanticDefinitions.map { definition =>
      ScalametaDefinitionAuthoring
        .author(definition)
        .fold(error => sys.error(error.message), identity)
    }
    val loweredDefinitions = semanticDefinitions.map { definition =>
      DefinitionUntypedLowering
        .lower(definition)
        .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    }
    val bridgedDefinitions = authoredDefinitions.map { source =>
      ScalametaDefinitionUntypedBridge
        .lower(source)
        .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    }

    assert(term.isInstanceOf[untpd.InfixOp])
    assert(bridgedTerm.isInstanceOf[untpd.InfixOp])
    assert(loweredType.isInstanceOf[untpd.AppliedTypeTree])
    assert(bridgedType.isInstanceOf[untpd.AppliedTypeTree])
    assert(loweredDefinitions.map(_.name.toString) == Vector("foo", "foo", "T"))
    assert(bridgedDefinitions.map(_.name.toString) == Vector("foo", "foo", "T"))
    assert(loweredDefinitions(0).isInstanceOf[untpd.ValDef])
    assert(loweredDefinitions(1).isInstanceOf[untpd.DefDef])
    assert(loweredDefinitions(2).isInstanceOf[untpd.TypeDef])
    (Vector(term, bridgedTerm, loweredType, bridgedType) ++ loweredDefinitions ++ bridgedDefinitions)
      .foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
      }

  private def parseDefinition(source: String): Defn =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
```
<!-- snippet:dotty-source-free:end -->

## Semantic Definition generated origin

`quasiquotes.definitions.dotty.DefinitionGeneratedOriginLowering.lower(definition,
virtualSourceName)(using Context)` is available in the current unpublished
exact-version Dotty-internal artifact. It accepts
public `SemanticDefinition` values for an immutable value, a parameterless
method, one or two ordinary method parameters, or a simple type alias. No
Scalameta conversion is involved.

The result is `Either[Failure, Lowered]`. `Lowered` exposes the positioned
`untpd.MemberDef`, `generatedSource`, `sourceFile`, and `virtualSourceName`;
callers cannot construct it directly. A simple alias such as `type T = List[Int]`
produces a `TypeDef` with a recursively positioned RHS. Repeating a valid call
produces identical source, path, and spans with fresh result, tree, and source
objects. All material trees use the returned generated source, carry contained
spans, and have no symbols or typed splices before Typer.

Callers branch on `Failure.code`; `detail` is diagnostic. The codes are
`MISSING_INPUT`, `MALFORMED_SEMANTIC_VALUE`, `UNSUPPORTED_SEMANTIC_VALUE`,
`SEMANTIC_ADAPTER_FAILED`, `INVALID_VIRTUAL_SOURCE`, `EXACT_LOWERING_FAILED`,
`GENERATED_ORIGIN_FAILED`, and `INTERNAL_INVARIANT_FAILED`. Validation checks a
missing definition first, then a missing path, then invalid non-null paths,
then semantic adaptation, exact generation, and positioned-result invariants.
Empty paths, surrounding whitespace, NUL, CR, LF, and paths changed by the
compiler's virtual-source authority are rejected.

The operation supplies generated origin only. The caller still owns target
admission, insertion, owner lifecycle, and typing. A generated alias can succeed
while an existing owner-append operation rejects `TypeDef`. This operation does
not change the historical Scalameta bridge or add an existing-owner rewrite API.

## Semantic Term generated origin

The additive development candidate `TermGeneratedOriginLowering.lower(term,
virtualSourceName)(using Context)` returns a positioned `untpd.Tree`, generated
source, fresh `SourceFile` and effective path directly from public `TermShape`.
It shares the richer checked source-free semantic authority once, then applies
additional decoded-name, constructor-segment and signed-receiver grouping
limits. `TermUntypedLowering` remains the broader source-free facade. The
[complete contract and executable example](SEMANTIC_TERM_GENERATED_ORIGIN_LOWERING.md)
state the seven failures, fixed precedence, binder/rich-Type limits and
recursive source identity/freshness requirements.

The Term sibling awaits independent implementation acceptance. Existing
Scalameta Term bridges remain separate; no Type generated-origin sibling or
public existing-tree transaction is selected.

## Checked generated-origin bridges

Source-free and generated-origin are different operations, not a boolean mode.
The generated-origin APIs return the raw tree together with deterministic
generated source and an effective virtual source. The Scalameta generated-origin Term bridge accepts more
completed binder/type-bearing families than the narrower source-free Scalameta
Term bridge; this is not full parity with the broader semantic Term facade.
The Scalameta Definition generated-origin bridge accepts the four concrete
val/def families; it continues rejecting simple aliases. The separate semantic
Definition generated-origin facade described above includes
simple aliases.

<!-- snippet:dotty-generated-origin:start -->
```scala
import _root_.quasiquotes.definitions.dotty.ScalametaDefinitionGeneratedOriginBridge
import _root_.quasiquotes.terms.dotty.ScalametaTermGeneratedOriginBridge

object DottyGeneratedOriginHelloWorld:
  def check(): Unit = withContext:
    val term = ScalametaTermGeneratedOriginBridge
      .lower(q"1 + 2", "SemanticGuideGeneratedTerm.scala")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val definition = ScalametaDefinitionGeneratedOriginBridge
      .lower(
        q"def foo(x: Int): String = x.toString".asInstanceOf[Defn.Def],
        "SemanticGuideGeneratedDefinition.scala"
      )
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val aliasFailure = ScalametaDefinitionGeneratedOriginBridge
      .lower(q"type T = Int".asInstanceOf[Defn.Type], "SemanticGuideGeneratedAlias.scala")
      .left
      .toOption
      .get

    assert(term.tree.source.path == term.virtualSourceName)
    assert(term.tree.span.start == 0 && term.tree.span.end == term.generatedSource.length)
    assert(definition.tree.source.path == definition.virtualSourceName)
    assert(
      definition.tree.span.start == 0 &&
        definition.tree.span.end == definition.generatedSource.length
    )
    assert(aliasFailure.code == "GENERATED_ORIGIN_FAMILY_UNSUPPORTED")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
```
<!-- snippet:dotty-generated-origin:end -->

## Generic versus specialized Definition bridges

The generic five-family bridge rejects the contextual method below because
generic methods and `using` clauses are outside its closed grammar. The
specialized `ContextualMethodPeerBridge` admits this exact bounded family and
returns a positioned generated method. It is not a fallback for arbitrary
generic Definition failures.

<!-- snippet:generic-specialized-definition:start -->
```scala
import _root_.quasiquotes.definitions.dotty.ContextualMethodPeerBridge

object GenericVsSpecializedDefinitionHelloWorld:
  def check(): Unit = withContext:
    val contextual =
      q"def apply[A](using inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]

    val genericFailure = ScalametaDefinitionUntypedBridge
      .lower(contextual)
      .left
      .toOption
      .get
    val specialized = ContextualMethodPeerBridge
      .lower(contextual, "SemanticGuideContextualApply.scala")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    assert(genericFailure.code == "NEUTRAL_PROJECTION_FAILED")
    assert(specialized.tree.name.toString == "apply")
    assert(specialized.tree.source.path == specialized.virtualSourceName)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
```
<!-- snippet:generic-specialized-definition:end -->

## Current versus planned quick reference

| Capability | Status |
| --- | --- |
| Public Term/Type Scalameta Projection and fresh Authoring | Current, bounded |
| Public binder-safe Term builders/views | Current, bounded |
| Public Core `SemanticDefinition` smart constructors/views | Current, bounded |
| Public generic Scalameta Definition Projection/Authoring over `SemanticDefinition` | Current, bounded to five families |
| Public project-semantic-value-to-Dotty Term/Type/Definition lowering facades | Current, bounded, source-free, exact-version for Dotty-facing artifacts |
| Public Scalameta-to-Dotty Term/Type/Definition bridges | Current, bounded, exact-version for Dotty-facing artifacts |
| Public exact existing-tree capture/rewrite algebra | **Planned** |
| Public `u*` syntax | Later optional, not selected |
| Remote `0.3.0` artifacts | Not released |

The checked snippets are one canonical copy in this page and one compiled copy
in the owning artifact. `tools/first-use/check-snippets.py` rejects byte drift;
the semantic tests use reprojection or structural/provenance assertions rather
than pretty-printing as the sole oracle.
