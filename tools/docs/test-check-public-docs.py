#!/usr/bin/env python3
"""Behavior tests for the public documentation boundary checker."""

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


CHECKER = Path(__file__).with_name("check-public-docs.py")


class PublicDocsCheckTest(unittest.TestCase):
    def make_fixture(
        self,
        root: Path,
        *,
        matrix_link: str = "docs/SYNTAX_SUPPORT_MATRIX.md",
        dqr_status: str = "Public now, exact bounded shape",
        readme_core_count: int = 1,
        readme_frontend_count: int = 14,
    ) -> None:
        docs = root / "docs"
        docs.mkdir(parents=True)
        definitions = root / "frontend/src/main/scala/quasiquotes/definitions"
        construct = root / "frontend/src/main/scala/quasiquotes/construct"
        matching = root / "frontend/src/main/scala/quasiquotes/matching"
        types = root / "frontend/src/main/scala/quasiquotes/types"
        definitions.mkdir(parents=True)
        construct.mkdir(parents=True)
        matching.mkdir(parents=True)
        types.mkdir(parents=True)

        (root / "README.md").write_text(
            f"[matrix]({matrix_link})\n"
            "[architecture](docs/ARCHITECTURE.md)\n"
            "[Macro-Paradise](https://github.com/DmytroMitin/macroparadise-scala3)\n"
            "[AUXify](https://github.com/DmytroMitin/AUXify-scala3)\n"
            "<!-- public-surface-table:start -->\n"
            "| Role | Interpolated syntax | Interpolator availability | Programmatic API | Function/API availability |\n"
            "| --- | --- | --- | --- | --- |\n"
            "| Term construction | `qr\"...\"` | Public now | `QuasiquoteBuilder.build(...)` | Public now |\n"
            "| Term pattern matching | `case qq\"...\"` | Public now | `QuasiPattern.term(...)`, `termOrThrow(...)` | Public now |\n"
            "| Type construction | `tqr\"...\"` | Public now | `QuasiTypequotes.tqr(...)` | Public research API |\n"
            "| Type pattern matching | `case tqq\"...\"` | Public now | `QuasiTypequotes.tqq(...)` / `QuasiTypePattern.*` | Public research API |\n"
            f"| Definition construction | `dqr\"def id(x: $parameterType): $resultType = x\"` | {dqr_status} | `DefinitionConstruction.*` | Public bounded compiler-free API |\n"
            "| Definition pattern matching | `case dqq\"def id(x: Int): Int = $body\"` | Public now, exact bounded shape | `DefinitionPattern.singleParameter(...)` | Public now, exact bounded shape |\n"
            "<!-- public-surface-table:end -->\n\n"
            "Static exact-one `dqq` returns `SingleParameterDefinitionPattern`; "
            "static structural templates return `DefinitionPatternExtractor`; "
            "dynamic/non-static `dqq` retains the single-parameter fallback. "
            "There is no `dqq2`, `dqq3`, or `dqq4` public API.\n\n"
            "The machine-readable [0.2.0 public API baseline](docs/api-baselines/0.2.0.tsv)\n"
            f"contains {readme_core_count} core and {readme_frontend_count} frontend Scaladoc-visible entries.\n",
            encoding="utf-8",
        )
        (root / "ROADMAP.md").write_text(
            "[north-star checkpoints](docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md)\n\n"
            "The current programme inventory is the current 798-row / 779-group standard surface.\n"
            "The direct Core source-free lowerer accepts direct identifiers, selections, and completed successive ordinary Apply clauses.\n\n"
            "Standard and opt-in typed-Scalameta scalar `qq` matching share the accepted bounded P3 local identity-method family. Opt-in typed-Scalameta `qr` construction admits the same one-method identity shape only as the complete source root; nested placement and synthetic provenance-free `qq` matching remain unavailable.\n\n"
            "| N1 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n"
            "| N2 | `BOUNDED_STANDARD_AND_TYPED_SCALAMETA_CONSTRUCTION_IMPLEMENTED` | dynamic-constructor ranked `tqq` matching is implemented on both typed frontends; construction and broader kinds remain narrower |\n"
            "| N3 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n"
            "| N4 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n"
            "| N5 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n",
            encoding="utf-8",
        )
        (docs / "ARCHITECTURE.md").write_text(
            "# Architecture\n\n"
            "The project owns one compiler-free semantic model. The current-Dotty frontend\n"
            "is the released default and the reference oracle; the typed Scalameta route is\n"
            "an explicit, released opt-in. The two typed routes must agree wherever they\n"
            "both advertise support. In the hybrid route, only a Scalameta parse failure may fall back.\n\n"
            "The compiler-free model is symbol-free, and no public symbol-quasiquote family\n"
            "is currently planned. Symbols and owners that are derivable from syntax belong to\n"
            "the typed backend's lowering plan. A pre-typer `untpd` backend must\n"
            "emit syntax without fabricating typed symbols.\n",
            encoding="utf-8",
        )
        (docs / "WHY_QUASIQUOTES.md").write_text(
            "[future checkpoints](NORTH_STAR_QUASIQUOTE_EXAMPLES.md)\n",
            encoding="utf-8",
        )
        (docs / "HYBRID_SCALAMETA_TERM_FRONTEND_EXPERIMENT.md").write_text(
            "The [canonical architecture](ARCHITECTURE.md) owns the shared status.\n",
            encoding="utf-8",
        )
        (docs / "NORTH_STAR_QUASIQUOTE_EXAMPLES.md").write_text(
            "# North-star quasiquote examples\n\n"
            "Syntax explicitly marked conceptual remains future notation.\n"
            "CURRENT_MANUAL_BASELINE_PROVED\n"
            "DESIGN_REQUIRED\n"
            "IMPLEMENTATION_REQUIRED\n\n"
            + "".join(
                f"## N{checkpoint}\n\n"
                "### Manual/current baseline\n\nBaseline.\n\n"
                + (
                    "### Implemented bounded source-like shape\n\n"
                    'tqr"$constructor[..$arguments]"\n'
                    "construction-only overload; caller-owned reflected class constructor; "
                    "runtime-length ordered `Seq[TypeRepr]`; validates arity, kinds; "
                    "directly in the caller's Quotes universe; exact constructor/argument identities.\n\n"
                    "### Remaining capabilities\n\n"
                    "General TypeLambda authoring, aliases-as-aliases, instance-dependent prefixes, "
                    "refinements, nontrivial constrained bounds and broader kind calculus remain "
                    "outside the selected slice. Both `tqq` frontends also match one exact root "
                    "constructor plus exactly one direct rank-2 argument slot over any compiler-presented `AppliedType`; scalar captures remain exact `TypeRepr` values and the ranked capture "
                    "is the original ordered `Seq[TypeRepr]`. Multiple rank-2 holes, rank 3, repeated or non-root constructor holes, "
                    "non-direct tuple/function positions, and general selected source-constructor syntax remain "
                    "unsupported. Matching remains broader than construction. No neutral runtime-sequence model is implied by the direct reflection path.\n\n"
                    if checkpoint == 2 else
                    "### Desired source-like shape\n\nShape.\n\n"
                    "### Required missing capabilities\n\nCapabilities.\n\n"
                )
                +
                "### Checkpoint criterion\n\nCriterion.\n\n"
                for checkpoint in range(1, 6)
            ),
            encoding="utf-8",
        )
        (docs / "SYNTAX_SUPPORT_MATRIX.md").write_text(
            "# Syntax support matrix\n\n"
            "SUPPORTED BOUNDED INTERNAL NOT_YET NOT_PLANNED\n\n"
            "| Sequence-Term arguments | Apply and New sequence support |\n"
            "| Ordered term capture extractor | Scalar slots bind q.reflect.Term; exactly one "
            "direct Apply or New sequence slot binds Seq[q.reflect.Term] |\n"
            "| Dynamic selected-member name | standard and opt-in typed-Scalameta `qq` selected-name scalar, ranked, and two distinct direct/unary matching |\n"
            "| Runtime-sequence reflected Type application | dynamic constructor matching |\n\n"
            "The source-owned local identity method row uses standard and opt-in typed-Scalameta `qq` matching with symbolic/Unicode source binders excluded. Opt-in typed-Scalameta `qr` construction is bounded to the complete source root; nested P3 placement and provenance-free matching of synthetic P3 trees remain unsupported.\n\n"
            "https://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html\n"
            "https://scalameta.org/docs/trees/quasiquotes\n\n"
            "Any change that adds, removes, or materially alters a term, type, or definition syntax family must update this matrix.\n",
            encoding="utf-8",
        )
        (docs / "CROSS_SURFACE_CAPABILITY_MATRIX.md").write_text(
            "# Cross-surface capability matrix\n\n"
            "| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |\n"
            "| --- | --- | --- | --- | --- | --- | --- | --- | --- |\n"
            "| Rank-2 Term arguments in Apply / one-list New | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` | `NOT_APPLICABLE` |\n"
            "| Runtime-length Type application arguments | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |\n"
            "| Dynamic selected-member name | `BOUNDED` | `BOUNDED` already-resolved Select.name with one-name/ranked forms plus standard two distinct direct/unary captures over compiler-presented `AppliedType` evidence | `BOUNDED` | `BOUNDED` Scalameta-primary shared matcher with accepted two distinct direct/unary parity | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |\n"
            "| Source-owned local identity method (P3) | `BOUNDED` | `BOUNDED` standard typed qq | `BOUNDED` complete-source-root local identity method only; callback-owned `MethodType`, `DefDef`, `Quotes`, source and Type sidecars | `BOUNDED` Scalameta-primary matching parity; symbolic and Unicode source binders excluded; original caller-universe `Term`; synthetic constructed P3 matching still requires explicit source TypeTree provenance | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_APPLICABLE` |\n"
            "| Existing-class zero-parameter ordinary methods | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `BOUNDED` `DefDef.paramss = Nil` projects as zero public term clauses; `List(Nil)` projects as one empty ordinary clause; both create zero parameter binders |\n"
            "| Existing-class two ordinary unary clauses | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `BOUNDED` exact `List(List(a), List(b))`; two `Ordinary` clauses at ordinals 0 and 1; distinct binders; preserved `List(1, 1)` topology and curried call syntax |\n"
            "| Existing-class contextual unary clauses | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `BOUNDED` C1 exact `List(List(context))` exposes one `Contextual` clause at ordinal 0; C2 exact `List(List(a), List(context))` exposes `Ordinary` at ordinal 0 then `Contextual` at ordinal 1; distinct binders; preserved `List(1)` or `List(1, 1)` topology and contextual call syntax |\n"
            "| Existing-class generic G1 unary method | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `BOUNDED` exact one unbounded private opaque `TypeDef` clause followed by one `Ordinary` strict unary term clause exposed at public term-clause ordinal 0; Type-independent primitive parameter/result Types and binder-aware body edits preserve the original Type clause, `TypeDef`, and bounds by identity; no public Type binder |\n"
            "| Existing-class generic G2 unary method | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `BOUNDED` exact one unbounded private opaque `TypeDef(A)` clause followed by one `Ordinary` strict unary term clause at public term-clause ordinal 0; dependent parameter/result `Ident(A)` semantics and replacement remain `UNSUPPORTED_STRUCTURE`; only the exact capture-local `x` reference is admitted as a replacement body; no public Type binder |\n"
            "| Existing-class canonical resolved Option[String] method Type write | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `BOUNDED` exact strict unary ordinary existing method; public `STypeApply(STypeResolved(scala.Option), List(STypeResolved(java.lang.String)))`; parameter-only, result-only, and combined capture-local body replacement; rooted alias-resistant output; semantic read projection remains unsupported |\n"
            "\n`TermUntypedLowering` and `TermGeneratedOriginLowering` preserve "
            "two or more successive ordinary argument clauses. "
            "`ScalametaTermProjection` and `ScalametaTermShapeAuthoring` preserve completed nested clauses. "
            "`CoreTermShapeUntypedLowerer` preserves completed nested clauses directly. "
            "`ScalametaTermUntypedBridge` and `ScalametaTermGeneratedOriginBridge` preserve completed nested clauses without delegation.\n",
            encoding="utf-8",
        )
        (docs / "SUPPORTED_SYNTAX_AND_LIMITATIONS.md").write_text(
            "# Supported syntax and limitations\n\n"
            "Same-spelling `dqq` uses structural specialization. Static exact-one "
            "templates retain `SingleParameterDefinitionPattern`; static structural "
            "templates use scalable `DefinitionPatternExtractor`; dynamic/non-static "
            "calls retain the single-parameter fallback.\n\n"
            "## Bounded term-pattern extractor\n\n"
            "Scalar slots bind `q.reflect.Term`; exactly one direct sequence slot in ordinary "
            "`Apply.arguments` or fixed one-list `New.arguments` binds "
            "`Seq[q.reflect.Term]`.\n\n"
            "Selected-member matching uses `qq\"$receiver.$selectedName\"`, `qq\"$receiver.$selectedName()\"`, `qq\"$receiver.$selectedName($argument)\"`, `qq\"$receiver.$selectedName(..$arguments)\"`, `qq\"$receiver.$selectedName($head, ..$tail)\"`, `qq\"$receiver.$selectedName(..$init, $last)\"`, and `qq\"$receiver.$selectedName($first, ..$middle, $last)\"`. "
            "It returns exact Term, one SelectedMemberName, and one Seq[Term] from the already-resolved `Select.name`; an already-resolved overload may match. "
            "The typed route is Scalameta-primary and reuses the shared matcher.\n\n"
            "Standard `qq` also admits `qq\"$receiver.$first.$second\"` and "
            "`qq\"$receiver.$first.$second($argument)\"`. Different Scala source "
            "binder names produce two independent `SelectedMemberName` fields in source "
            "order; target member names may be equal, and an ordinary "
            "`if first == second` guard runs only after successful extraction. Exact two-name and direct N=3..8 candidates require recoverable original source; "
            "legacy scalar, one-name, ranked, mixed, and local-method candidates bypass "
            "source-position inspection. A new-family candidate without source fails closed. Opt-in typed-Scalameta `qq` admits the "
            "same exact two-name direct/unary contract. Native repeated "
            "same-binder patterns fail with E005; neither `#repeat` nor `qq2` is public "
            "syntax.\n\n"
            "Dynamic Type matching admits `tqq\"$constructor[..$arguments]\"`, `tqq\"$constructor[$head, ..$tail]\"`, `tqq\"$constructor[..$init, $last]\"`, `tqq\"$constructor[$first, ..$middle, $last]\"`, and `tqq\"$constructor[Int, ..$tail]\"` over any compiler-presented `AppliedType`; construction remains narrower and exact TypeRepr plus Seq[TypeRepr] identities are preserved. Matching is broader than the narrower construction contract.\n"
            "\nStandard and opt-in typed-Scalameta scalar `qq` matching share one bounded P3 local identity-method family. "
            "The opt-in route uses Scalameta-primary compilation. Both routes match one local identity method "
            "with equal explicit `Int`, `String`, or `Boolean` parameter and result Types; "
            "source method and parameter binders use the accepted ASCII simple-binder policy, "
            "excluding symbolic and Unicode spellings, while target method and parameter "
            "spellings remain alpha-equivalent by role. The only capture is the original "
            "caller-universe `quotes.reflect.Term` argument. Opt-in typed-Scalameta `qr` "
            "construction accepts that same fixed equal-Type identity shape only as the complete "
            "source root, using callback-owned `MethodType`, `DefDef`, `Quotes`, and source/Type "
            "sidecars. Nested P3 in outer If, Apply, Tuple, P2 initializer, or Lambda body is "
            "rejected. Immediate `qq` matching of a synthetically constructed P3 remains "
            "restricted because its TypeTrees lack explicit source provenance; broader blocks, "
            "methods, clauses, captures, and ranked P3 remain unsupported.\n",
            encoding="utf-8",
        )
        (docs / "EXISTING_CLASS_UNTYPED_REWRITE.md").write_text(
            "# Existing-class untyped rewrite\n\n"
            "By-name parameter-Type replacement preserves a fresh `ByNameTypeTree` wrapper. "
            "Repeated parameter-Type replacement preserves a fresh `PostfixOp` wrapper and "
            "the original `Ident(*)` marker by identity. The admitted family includes "
            "raw `DefDef.paramss = Nil`, which projects as zero public term parameter clauses, "
            "and raw `List(Nil)`, which projects as one empty ordinary clause; both create zero "
            "`ParameterRef` binders. It also retains "
            "the one ordinary clause of one or two parameters family and admits exactly two "
            "ordinary unary clauses with distinct parameter names. Those clauses remain separate "
            "at ordinals 0 and 1 with independent binders and `List(1, 1)` topology; parameter-Type, "
            "result-Type, binder-aware body, and combined atomic edits preserve curried call syntax. "
            "For the two-ordinary-clause family, duplicate names, generic methods, defaulted "
            "parameters, erased parameters, and by-name or repeated parameters remain "
            "unsupported. The separate contextual family admits C1 one contextual unary clause "
            "at ordinal 0 and C2 one ordinary unary clause at ordinal 0 followed by one contextual "
            "unary clause at ordinal 1. Each clause exposes its true `Ordinary` or `Contextual` "
            "kind, and `ParameterClauseKind.Contextual.code == \"CONTEXTUAL\"`. C1 preserves "
            "`List(List(context))` and `List(1)`; C2 preserves "
            "`List(List(a), List(context))`, distinct binders, `List(1, 1)`, and contextual call "
            "syntax. Contextual-before-ordinary, two contextual clauses, empty or wide contextual "
            "clauses, third clauses, duplicate names, Scala 2 implicit-style clauses, generic, "
            "defaulted, erased, by-name, and repeated contextual forms remain unsupported. Mixed "
            "The separate G1 family admits exact one unbounded private opaque `TypeDef` clause "
            "followed by one `Ordinary` strict unary term clause exposed at public term-clause ordinal 0. "
            "Type-independent primitive parameter/result Types and binder-aware body edits preserve "
            "the original Type clause, `TypeDef`, and bounds by identity; no public Type binder is exposed. "
            "The separate G2 family admits exact one unbounded private opaque `TypeDef(A)` clause "
            "followed by one `Ordinary` strict unary term clause at public term-clause ordinal 0. "
            "Dependent parameter/result `Ident(A)` semantics and replacement remain "
            "`UNSUPPORTED_STRUCTURE`; only the exact capture-local `x` reference is admitted as a "
            "replacement body. The Type clause, `TypeDef`, bounds, dependent Type nodes, source, "
            "span, method, and owner identities remain preserved, and no public Type binder is exposed. "
            "Primitive-shadowing, bounded, wide, or multiple Type clauses, and contextual, defaulted, "
            "by-name, or repeated adjacent forms remain fail-closed. "
            "edits compose across two distinct admitted "
            "methods of the same captured class while preserving independent method/parameter "
            "refs.\n\n"
            "The bounded two-method transaction may combine those Type edits with capture-local "
            "omission of one direct member and append of one ordinary accepted public "
            "`SemanticDefinition`. Surviving original members retain exact order and identity, "
            "and the generated member is last with deterministic generated-source provenance. "
            "Generated strict `val` initializers must be appended in dependency order; ordinary "
            "Typer and TASTy success with no warnings does not make a forward eager reference "
            "safe, and the JVM may observe the default value. "
            "Both legal immutable plan-building orders are supported, and invalid combinations "
            "fail before a result is exposed.\n\n"
            "The canonical applied method-Type write family is limited to one strict unary "
            "ordinary existing method and the exact public value "
            "`STypeApply(STypeResolved(ResolvedTypeNameId(Vector(Package(scala)), Option)), "
            "List(STypeResolved(ResolvedTypeNameId(Vector(Package(java), Package(lang)), String))))`. "
            "It lowers to `_root_.scala.Option[_root_.java.lang.String]` for parameter-only, "
            "result-only, and combined capture-local body replacement. Public construction uses "
            "`ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)`, "
            "`ResolvedTypeNameId`, `TypeNormalForm.STypeResolved`, and "
            "`TypeNormalForm.STypeApply`. Bare `STypeIdent(\"Option\")` or "
            "`STypeIdent(\"String\")`, nested or other applied constructors, G2-dependent Type "
            "replacement, and general `@addOption` expression wrapping remain unsupported. "
            "Recapture permits fresh write transactions, but `TypeSlotView.semantic` still "
            "returns `UNSUPPORTED_STRUCTURE` for the rooted applied Type.\n\n"
            "This does not authorize arbitrary calling-mode rewriting. It does not authorize "
            "arbitrary multiple methods or broader contextual/generic method shapes. Generated "
            "sibling-reference resolution remains separate. This does not authorize general "
            "transactional rollback across later compiler phases.\n",
            encoding="utf-8",
        )
        (docs / "PROJECTION_LOWERING_BRIDGE_MATRIX.md").write_text(
            "# Projection lowering bridge matrix\n\n"
            "The public existing-class transaction distinguishes parameterless `Nil` as zero "
            "public term clauses from explicit-empty `List(Nil)` as one empty ordinary clause, "
            "with zero parameter binders and preserved call syntax. It also preserves exact "
            "`List(List(a), List(b))` as two separate ordinary unary clauses with distinct binders, "
            "ordinals 0 and 1, `List(1, 1)` topology, and curried call syntax. It supports the bounded two-method "
            "calling-mode family. Separately, C1 preserves one `Contextual` clause at ordinal 0, "
            "while C2 preserves `Ordinary` at ordinal 0 followed by `Contextual` at ordinal 1, "
            "without flattening either clause topology; the contextual kind code is `CONTEXTUAL`. "
            "G1 preserves one private opaque unbounded `TypeDef` clause and exposes only one `Ordinary` "
            "term clause at public ordinal 0. Type-independent primitive parameter/result/body edits "
            "preserve original `TypeDef` and bounds by identity. G2 preserves one private opaque "
            "unbounded `TypeDef(A)` clause and exposes only one `Ordinary` term clause at public ordinal 0; "
            "dependent parameter/result `Ident(A)` semantics and replacement remain "
            "`UNSUPPORTED_STRUCTURE`, while only the exact capture-local `x` reference is admitted as a "
            "replacement body. No public Type-binder carrier exists. "
            "It supports the bounded two-method "
            "calling-mode edit plus capture-local omit and generated SemanticDefinition append "
            "composition in one immutable plan. It is not a universal owner editor.\n",
            encoding="utf-8",
        )
        (docs / "COMPATIBILITY.md").write_text(
            "# Compatibility\n\n"
            "The Scala/TASTy `dqq` declaration is now a transparent-inline selector. "
            "The historical erased JVM descriptor returning "
            "`SingleParameterDefinitionPattern` is preserved by a source-hidden bridge. "
            "The selector replacement is a new experimental 0.x-minor-class change.\n",
            encoding="utf-8",
        )
        baselines = docs / "api-baselines"
        baselines.mkdir()
        (baselines / "0.2.0.tsv").write_text(
            "module\towner\tkind\tname\tsignature\n"
            "frontend\tquasiquotes.construct.Quasiquotes\tdef\tqr\tqr signature\n"
            "frontend\tquasiquotes.construct.QuasiquoteBuilder\tdef\tbuild\tbuild signature\n"
            "frontend\tquasiquotes.matching.QuasiPattern\tdef\tterm\tterm signature\n"
            "frontend\tquasiquotes.matching.QuasiPattern\tdef\ttermOrThrow\ttermOrThrow signature\n"
            "frontend\tquasiquotes.matching.QuasiPattern\tdef\tqq\tqq(using q: Quotes): TermPatternExtractor[q.reflect.Term]\n"
            "frontend\tquasiquotes.matching.TermPatternExtractor\tdef\tunapplySeq\tunapply signature\n"
            "frontend\tquasiquotes.types.QuasiTypequotes\tdef\ttqr\ttqr signature\n"
            "frontend\tquasiquotes.types.QuasiTypequotes\tdef\ttqq\ttqq signature\n"
            "frontend\tquasiquotes.types.TypePatternExtractor\tdef\tunapplySeq\tunapply signature\n"
            "frontend\tquasiquotes.construct.Quasiquotes\tdef\tdqr\tdqr signature\n"
            "frontend\tquasiquotes.matching.DefinitionPattern\tdef\tsingleParameter\tsingle signature\n"
            "frontend\tquasiquotes.matching.DefinitionPattern\tdef\tdqq\tdqq signature\n"
            "frontend\tquasiquotes.matching.SingleParameterDefinitionPattern\tdef\tmatchDefinition\tmatch signature\n"
            "frontend\tquasiquotes.matching.SingleParameterDefinitionPattern\tdef\tunapply\tunapply signature\n"
            "core\tquasiquotes.publicapi.DefinitionConstruction\tdef\ttwoParameterMethod\ttwo signature\n",
            encoding="utf-8",
        )
        (definitions / "DefinitionQuasiquotes.scala").write_text(
            "private[quasiquotes] object DefinitionQuasiquotes:\n  def dqr = ()\n",
            encoding="utf-8",
        )
        (construct / "Quasiquotes.scala").write_text(
            "object Quasiquotes:\n"
            "  def dqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.DefDef = ???\n",
            encoding="utf-8",
        )
        (matching / "QuasiPattern.scala").write_text(
            "object QuasiPattern:\n"
            "  def term = ()\n"
            "  def termOrThrow = ()\n"
            "  private[matching] def scalarExtractor(sc: StringContext)"
            "(using q: Quotes): TermPatternExtractor[q.reflect.Term] = ???\n"
            "  @targetName(\"qq\")\n"
            "  private[matching] def qqLegacy(sc: StringContext)"
            "(using q: Quotes): TermPatternExtractor[q.reflect.Term] =\n"
            "    scalarExtractor(sc)\n"
            "  extension (inline sc: StringContext)\n"
            "    transparent inline def qq(using q: Quotes) = extractor(sc)\n",
            encoding="utf-8",
        )
        (matching / "QuasiPatternMacro.scala").write_text(
            "object QuasiPatternMacro:\n"
            "  def scalar = QuasiPattern.scalarExtractor(context)\n"
            "  def ranked = RankedTermPatternExtractorFactory.extractor(context)\n",
            encoding="utf-8",
        )
        (matching / "TermPatternExtractor.scala").write_text(
            "final class TermPatternExtractor[T]:\n  def unapplySeq(value: T): Option[Seq[T]] = ???\n",
            encoding="utf-8",
        )
        (matching / "RankedTermPatternExtractor.scala").write_text(
            "final class RankedTermPatternExtractor[T, Captures <: Tuple](extract: T => Option[Captures]):\n"
            "  def unapply(value: T): Option[Captures] = extract(value)\n",
            encoding="utf-8",
        )
        (matching / "DefinitionPattern.scala").write_text(
            "object DefinitionPattern:\n"
            "  private[matching] def singleParameterExtractor(sc: StringContext)"
            "(using q: Quotes): SingleParameterDefinitionPattern = ???\n"
            "  private[matching] def twoParameterExtractor(sc: StringContext)"
            "(using q: Quotes): DefinitionPatternExtractor = ???\n"
            "  @targetName(\"dqq\")\n"
            "  private[matching] def dqqLegacy(sc: StringContext)"
            "(using q: Quotes): SingleParameterDefinitionPattern =\n"
            "    singleParameterExtractor(sc)\n"
            "  extension (inline sc: StringContext)\n"
            "    transparent inline def dqq(using q: Quotes) = extractor(sc)\n"
            "  def singleParameter(source: String) = ???\n"
            "final class SingleParameterDefinitionPattern:\n"
            "  def matchDefinition(using q: Quotes)(target: q.reflect.DefDef) = ???\n"
            "  def unapply(using q: Quotes)(target: q.reflect.DefDef) = ???\n",
            encoding="utf-8",
        )
        (matching / "DefinitionPatternMacro.scala").write_text(
            "object DefinitionPatternMacro:\n"
            "  def single = DefinitionPattern.singleParameterExtractor(context)\n"
            "  def two = DefinitionPattern.twoParameterExtractor(context)\n",
            encoding="utf-8",
        )
        (matching / "DefinitionPatternExtractor.scala").write_text(
            "final class DefinitionPatternExtractor private (expected: Any):\n"
            "  def unapply(using q: Quotes)(target: q.reflect.DefDef) = ???\n",
            encoding="utf-8",
        )
        (types / "QuasiTypequotes.scala").write_text(
            "object QuasiTypequotes:\n"
            "  def tqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.TypeRepr = ???\n"
            "  private[types] def scalarExtractor(sc: StringContext)(using q: Quotes): TypePatternExtractor[q.reflect.TypeRepr] = ???\n"
            "  extension (inline sc: StringContext)\n"
            "    transparent inline def tqq(using q: Quotes) = extractor(sc)\n"
            "  @targetName(\"tqq\")\n"
            "  private[types] def tqqLegacy(sc: StringContext)(using q: Quotes): TypePatternExtractor[q.reflect.TypeRepr] =\n"
            "    scalarExtractor(sc)\n",
            encoding="utf-8",
        )
        (types / "TypePatternExtractor.scala").write_text(
            "final class TypePatternExtractor[T]:\n"
            "  def unapplySeq(value: T): Option[Seq[T]] = ???\n",
            encoding="utf-8",
        )
        (types / "RankedTypePatternExtractor.scala").write_text(
            "final class RankedTypePatternExtractor[T, Captures <: Tuple](extract: T => Option[Captures]):\n"
            "  def unapply(value: T): Option[Captures] = extract(value)\n",
            encoding="utf-8",
        )

        p = docs / "SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
        p.write_text(p.read_text() + "\nBoth typed frontends support direct flat selected-member chains at depths N=1..8.\n")

    def run_checker(self, root: Path) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), "--root", str(root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def test_accepts_truthful_surface_table_matrix_and_links(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("PUBLIC_DOCUMENTATION_BOUNDARY_PASS", result.stdout)

    def test_rejects_stale_typed_scalameta_rank2_term_matrix_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                matrix.read_text(encoding="utf-8").replace(
                    "| Rank-2 Term arguments in Apply / one-list New | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` |",
                    "| Rank-2 Term arguments in Apply / one-list New | `BOUNDED` | `BOUNDED` | `NOT_YET` | `NOT_YET` |",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("rank-2 Term matrix", result.stderr)

    def test_rejects_missing_standard_rank2_type_matching_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                matrix.read_text(encoding="utf-8").replace(
                    "| Runtime-length Type application arguments | `BOUNDED` | `BOUNDED` |",
                    "| Runtime-length Type application arguments | `BOUNDED` | `NOT_YET` |",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("rank-2 Type matrix", result.stderr)

    def test_rejects_missing_typed_scalameta_rank2_type_parity_claim(self) -> None:
        for replacement in (
            "| Runtime-length Type application arguments | `BOUNDED` | `BOUNDED` | `NOT_YET` | `BOUNDED` |",
            "| Runtime-length Type application arguments | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` |",
        ):
            with self.subTest(replacement=replacement), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.make_fixture(root)
                matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
                matrix.write_text(
                    matrix.read_text(encoding="utf-8").replace(
                        "| Runtime-length Type application arguments | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` |",
                        replacement,
                    ),
                    encoding="utf-8",
                )

                result = self.run_checker(root)

                self.assertEqual(result.returncode, 1)
                self.assertIn("rank-2 Type matrix", result.stderr)

    def test_rejects_missing_transparent_inline_qq_selector(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "transparent inline def qq(using q: Quotes) = extractor(sc)",
                    "def qqCurrent(using q: Quotes) = extractor(sc)",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("transparent-inline selector contract", result.stderr)

    def test_rejects_missing_scalar_qq_extractor_route(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/QuasiPatternMacro.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "QuasiPattern.scalarExtractor", "QuasiPattern.removedScalarRoute"
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("scalar extractor route contract", result.stderr)

    def test_rejects_missing_legacy_qq_jvm_bridge(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "@targetName(\"qq\")", "@targetName(\"qqRemoved\")"
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("legacy JVM bridge contract", result.stderr)

    def test_rejects_missing_ranked_qq_extractor_contract(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/RankedTermPatternExtractor.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "final class RankedTermPatternExtractor", "final class RemovedRankedExtractor"
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("ranked extractor contract", result.stderr)

    def test_rejects_stale_pre_q002_qq_source_shape_as_current_evidence(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
            source.write_text(
                "object QuasiPattern:\n"
                "  def term = ()\n"
                "  def termOrThrow = ()\n"
                "  def qq(using q: Quotes): TermPatternExtractor[q.reflect.Term] = ???\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("transparent-inline selector contract", result.stderr)
            self.assertIn("legacy JVM bridge contract", result.stderr)

    def test_rejects_missing_ranked_qq_documentation_contract(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                "# Supported syntax and limitations\n\nScalar qq matching only.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("scalar/ranked documentation contract", result.stderr)


    def test_rejects_missing_transparent_inline_tqq_selector(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/types/QuasiTypequotes.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "transparent inline def tqq(using q: Quotes) = extractor(sc)",
                    "def tqqCurrent(using q: Quotes) = extractor(sc)",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("tqq transparent-inline selector contract", result.stderr)

    def test_rejects_missing_legacy_tqq_jvm_bridge(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/types/QuasiTypequotes.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "@targetName(\"tqq\")", "@targetName(\"tqqRemoved\")"
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("tqq legacy JVM bridge contract", result.stderr)

    def test_rejects_missing_ranked_tqq_extractor_contract(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/types/RankedTypePatternExtractor.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "final class RankedTypePatternExtractor", "final class RemovedRankedTypeExtractor"
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("ranked tqq extractor contract", result.stderr)
    def test_rejects_missing_relative_documentation_link(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, matrix_link="docs/MISSING.md")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("missing relative link", result.stderr)

    def test_rejects_stale_internal_only_dqr_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, dqr_status="Internal research, not public")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("surface row mismatch", result.stderr)

    def test_rejects_stale_one_parameter_only_dqq_documentation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8").replace(
                    "static structural templates return `DefinitionPatternExtractor`; ",
                    "static structural templates are undocumented; ",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("Q012RR definition-pattern documentation contract", result.stderr)

    def test_rejects_missing_dqq_jvm_and_tasty_compatibility_accounting(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            compatibility = root / "docs/COMPATIBILITY.md"
            compatibility.write_text(
                "# Compatibility\n\nDefinition compatibility is undocumented.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("Q012RR dqq compatibility accounting", result.stderr)

    def test_rejects_missing_related_project_link(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8").replace(
                    "https://github.com/DmytroMitin/AUXify-scala3",
                    "https://example.invalid/auxify",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("README missing related-project link", result.stderr)

    def test_rejects_missing_canonical_architecture_fact(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            architecture = root / "docs/ARCHITECTURE.md"
            architecture.write_text(
                architecture.read_text(encoding="utf-8").replace(
                    "only a Scalameta parse failure may fall back",
                    "fallback policy omitted",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("canonical architecture missing fact", result.stderr)

    def test_rejects_missing_north_star_checkpoint(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            north_star = root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md"
            north_star.write_text(
                north_star.read_text(encoding="utf-8").replace("## N4", "## omitted"),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("north-star document missing checkpoint", result.stderr)

    def test_rejects_checkpoint_without_required_structure(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            north_star = root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md"
            north_star.write_text(
                north_star.read_text(encoding="utf-8").replace(
                    "### Checkpoint criterion\n\nCriterion.",
                    "### Criterion omitted\n\nOmitted.",
                    1,
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("north-star checkpoint N1 missing section", result.stderr)

    def test_rejects_silently_completed_roadmap_checkpoint(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            roadmap = root / "ROADMAP.md"
            roadmap.write_text(
                roadmap.read_text(encoding="utf-8").replace(
                    "| N5 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |",
                    "| N5 | `CHECKPOINT_COMPLETE` |",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("roadmap checkpoint N5 has invalid status", result.stderr)

    def test_rejects_obsolete_future_only_n2_status(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            roadmap = root / "ROADMAP.md"
            roadmap.write_text(roadmap.read_text().replace(
                "BOUNDED_STANDARD_AND_TYPED_SCALAMETA_CONSTRUCTION_IMPLEMENTED",
                "DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED",
            ))
            result = self.run_checker(root)
            self.assertEqual(result.returncode, 1)
            self.assertIn("roadmap checkpoint N2 has invalid status", result.stderr)

    def test_rejects_n2_without_direct_reflection_or_remaining_exclusions(self) -> None:
        for text in (
            "directly in the caller's Quotes universe",
            "exact constructor/argument identities",
            "General TypeLambda authoring",
            "Both `tqq` frontends also match one exact root constructor plus exactly one direct rank-2 argument slot",
            "scalar captures remain exact `TypeRepr` values and the ranked capture is the original ordered `Seq[TypeRepr]`",
            "Multiple rank-2 holes, rank 3, repeated or non-root constructor holes, non-direct tuple/function positions",
            "No neutral runtime-sequence model is implied",
        ):
            with self.subTest(text=text), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.make_fixture(root)
                doc = root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md"
                doc.write_text(doc.read_text().replace(text, "omitted"))
                result = self.run_checker(root)
                self.assertEqual(result.returncode, 1)
                self.assertIn("north-star checkpoint N2 missing bounded contract", result.stderr)

    def test_rejects_n2_promoted_to_complete(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            roadmap = root / "ROADMAP.md"
            roadmap.write_text(roadmap.read_text().replace(
                "BOUNDED_STANDARD_AND_TYPED_SCALAMETA_CONSTRUCTION_IMPLEMENTED",
                "BOUNDED_STANDARD_AND_TYPED_SCALAMETA_CONSTRUCTION_IMPLEMENTED`, `CHECKPOINT_COMPLETE",
            ))
            result = self.run_checker(root)
            self.assertEqual(result.returncode, 1)
            self.assertIn("roadmap checkpoint N2 has invalid status", result.stderr)

    def test_rejects_bounded_n2_status_applied_to_n1(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            roadmap = root / "ROADMAP.md"
            roadmap.write_text(roadmap.read_text().replace(
                "| N1 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |",
                "| N1 | `BOUNDED_STANDARD_AND_TYPED_SCALAMETA_CONSTRUCTION_IMPLEMENTED` |",
            ))
            result = self.run_checker(root)
            self.assertEqual(result.returncode, 1)
            self.assertIn("roadmap checkpoint N1 has invalid status", result.stderr)

    def test_rejects_scalameta_status_doc_without_canonical_link(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            experiment = root / "docs/HYBRID_SCALAMETA_TERM_FRONTEND_EXPERIMENT.md"
            experiment.write_text("standalone status narrative\n", encoding="utf-8")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("Scalameta experiment missing canonical architecture link", result.stderr)

    def test_ignores_markdown_link_shapes_inside_code(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8")
                + "\nInline: `def apply[A](using value: Show[A]) = value`\n"
                + "```scala\ndef apply[A](using value: Show[A]) = value\n```\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 0, result.stderr)

    def test_rejects_stale_frontend_api_count(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, readme_frontend_count=13)

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("public API count drift", result.stderr)
            self.assertIn("README core=1 frontend=13", result.stderr)
            self.assertIn("baseline core=1 frontend=14", result.stderr)

    def test_rejects_stale_core_api_count(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, readme_core_count=2)

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("public API count drift", result.stderr)
            self.assertIn("README core=2 frontend=14", result.stderr)
            self.assertIn("baseline core=1 frontend=14", result.stderr)

    def test_rejects_baseline_growth_without_readme_count_update(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            baseline = root / "docs/api-baselines/0.2.0.tsv"
            baseline.write_text(
                baseline.read_text(encoding="utf-8")
                + "core\tquasiquotes.publicapi.AddedApi\tdef\tadded\tadded signature\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("public API count drift", result.stderr)
            self.assertIn("README core=1 frontend=14", result.stderr)
            self.assertIn("baseline core=2 frontend=14", result.stderr)

    def test_rejects_unexpected_public_api_module(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            baseline = root / "docs/api-baselines/0.2.0.tsv"
            baseline.write_text(
                baseline.read_text(encoding="utf-8")
                + "experimental\tquasiquotes.experimental.Api\tdef\tvalue\tvalue signature\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn(
                "public API count contract requires exactly core and frontend modules",
                result.stderr,
            )
            self.assertIn("experimental", result.stderr)


    def test_rejects_missing_c065_selected_member_layout(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8").replace(
                    "qq\"$receiver.$selectedName(..$init, $last)\"",
                    "selected layout omitted",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C065 selected-member documentation missing layout", result.stderr)

    def test_rejects_missing_c069_completed_apply_coordinated_surface(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            (root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md").write_text(
                "completed nested Apply is supported everywhere\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C069 completed Apply coordinated surface missing", result.stderr)

    def test_rejects_stale_c069_roadmap_backend_and_inventory_truth(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            roadmap = root / "ROADMAP.md"
            roadmap.write_text(
                roadmap.read_text(encoding="utf-8")
                + "\nThe current programme inventory is the current 794-row / 775-group standard surface.\n"
                + "The exact backend intentionally remains narrower: only the integer/infix core family lowers to source-free trees; Identifier, Select, and Apply fail.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C069 roadmap truth stale", result.stderr)

    def test_rejects_missing_c068_standard_p3_match_contract(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8").replace(
                    "Both routes match one local identity method",
                    "binder matching omitted",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C068 standard P3 matching contract missing", result.stderr)

    def test_rejects_stale_c070_hybrid_p3_non_acceptance_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8")
                + "\nA typed-Scalameta P3 matcher is present but is not an accepted documented public capability.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C070 stale hybrid P3 acceptance claim", result.stderr)

    def test_rejects_missing_c070_hybrid_p3_lexical_and_identity_boundary(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8").replace(
                    "excluding symbolic and Unicode spellings",
                    "accepting every compiler binder spelling",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C070 hybrid P3 bounded contract missing", result.stderr)

    def test_rejects_c073_hybrid_p3_not_yet_status(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                matrix.read_text(encoding="utf-8").replace(
                    "| `BOUNDED` complete-source-root local identity method only",
                    "| `NOT_YET` complete-source-root local identity method only",
                    1,
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C073 hybrid P3 matrix", result.stderr)

    def test_rejects_c073_nested_hybrid_p3_construction_overclaim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8")
                + "\nNested typed-Scalameta P3 construction is supported inside If and Lambda bodies.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C073 hybrid P3 construction overclaim", result.stderr)

    def test_rejects_c073_synthetic_p3_matching_overclaim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8")
                + "\nSynthetic constructed P3 trees match `qq` without explicit source TypeTree provenance.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C073 synthetic P3 matching overclaim", result.stderr)

    def test_rejects_c073_nil_and_list_nil_conflation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8").replace(
                    "`DefDef.paramss = Nil`, which projects as zero public term parameter clauses, "
                    "and raw `List(Nil)`, which projects as one empty ordinary clause",
                    "`DefDef.paramss = Nil` and `List(Nil)` as the same zero-clause topology",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C073 zero-parameter topology contract missing", result.stderr)
            self.assertIn("C073 Nil/List(Nil) conflation", result.stderr)

    def test_rejects_c073_stale_one_or_two_parameter_only_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8")
                + "\nThe facade admits only one ordinary clause of one or two parameters.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C073 stale existing-method facade claim", result.stderr)

    def test_rejects_unaccepted_successor_shipping_claims(self) -> None:
        claims = (
            "Synthetic P3 qr trees are accepted by qq matching without source provenance.",
            "Repeated same-logical selected-name construction is a supported public capability.",
        )
        for claim in claims:
            with self.subTest(claim=claim):
                with tempfile.TemporaryDirectory() as directory:
                    root = Path(directory)
                    self.make_fixture(root)
                    readme = root / "README.md"
                    readme.write_text(
                        readme.read_text(encoding="utf-8") + f"\n{claim}\n",
                        encoding="utf-8",
                    )

                    result = self.run_checker(root)

                    self.assertEqual(result.returncode, 1)
                    self.assertIn("unaccepted shipping claim", result.stderr)

    def test_accepts_c074_two_ordinary_unary_clause_shipping_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8")
                + "\nTwo ordinary unary clauses are supported by ExistingClassUntypedRewrite.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("PUBLIC_DOCUMENTATION_BOUNDARY_PASS", result.stdout)

    def test_rejects_missing_c074_two_ordinary_unary_clause_row(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                "\n".join(
                    line
                    for line in matrix.read_text(encoding="utf-8").splitlines()
                    if not line.startswith("| Existing-class two ordinary unary clauses |")
                )
                + "\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("two ordinary unary clause matrix", result.stderr)

    def test_rejects_c074_flattened_two_clause_topology(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8").replace(
                    "Those clauses remain separate at ordinals 0 and 1 with independent binders and `List(1, 1)` topology",
                    "Those clauses flatten into one binary clause with shared binder topology",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("two-clause topology contract missing", result.stderr)

    def test_rejects_c074_contextual_or_duplicate_name_two_clause_overclaim(self) -> None:
        claims = (
            "All contextual second clauses are supported by ExistingClassUntypedRewrite.",
            "Duplicate parameter names are supported in the two-clause family.",
        )
        for claim in claims:
            with self.subTest(claim=claim), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.make_fixture(root)
                readme = root / "README.md"
                readme.write_text(
                    readme.read_text(encoding="utf-8") + f"\n{claim}\n",
                    encoding="utf-8",
                )

                result = self.run_checker(root)

                self.assertEqual(result.returncode, 1)
                self.assertIn("two-clause adjacent-family overclaim", result.stderr)

    def test_rejects_missing_c076_contextual_clause_row(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                "\n".join(
                    line
                    for line in matrix.read_text(encoding="utf-8").splitlines()
                    if not line.startswith("| Existing-class contextual unary clauses |")
                )
                + "\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C076 contextual clause matrix", result.stderr)

    def test_rejects_c076_swapped_clause_kinds_and_ordinals(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                matrix.read_text(encoding="utf-8").replace(
                    "exposes `Ordinary` at ordinal 0 then `Contextual` at ordinal 1",
                    "exposes `Contextual` at ordinal 0 then `Ordinary` at ordinal 1",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C076 contextual clause matrix detail", result.stderr)

    def test_rejects_missing_c076_contextual_code_truth(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8").replace(
                    "`ParameterClauseKind.Contextual.code == \"CONTEXTUAL\"`",
                    "`ParameterClauseKind.Contextual`",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C076 contextual rewrite contract missing", result.stderr)

    def test_rejects_c076_stale_contextual_not_yet_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8")
                + "\nContextual existing-tree rewrite is `NOT_YET`.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C076 stale contextual availability claim", result.stderr)

    def test_rejects_c076_broad_contextual_or_implicit_admission(self) -> None:
        claims = (
            "All contextual method shapes are supported by ExistingClassUntypedRewrite.",
            "Scala 2 implicit-style clauses are supported by ExistingClassUntypedRewrite.",
            "Generic contextual methods are supported by ExistingClassUntypedRewrite.",
            "Defaulted contextual parameters are supported by ExistingClassUntypedRewrite.",
            "Erased contextual parameters are supported by ExistingClassUntypedRewrite.",
        )
        for claim in claims:
            with self.subTest(claim=claim), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.make_fixture(root)
                readme = root / "README.md"
                readme.write_text(
                    readme.read_text(encoding="utf-8") + f"\n{claim}\n",
                    encoding="utf-8",
                )

                result = self.run_checker(root)

                self.assertEqual(result.returncode, 1)
                self.assertIn("C076 contextual adjacent-family overclaim", result.stderr)

    def test_rejects_missing_c076_accepted_u068_g1_row(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                "\n".join(
                    line
                    for line in matrix.read_text(encoding="utf-8").splitlines()
                    if not line.startswith("| Existing-class generic G1 unary method |")
                )
                + "\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C076 accepted U068 G1 matrix", result.stderr)

    def test_rejects_missing_c079_accepted_u069_g2_row(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                "\n".join(
                    line
                    for line in matrix.read_text(encoding="utf-8").splitlines()
                    if not line.startswith("| Existing-class generic G2 unary method |")
                )
                + "\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C079 accepted U069 G2 matrix", result.stderr)

    def test_rejects_missing_c079_exact_x_body_only_contract(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8").replace(
                    "only the exact capture-local `x` reference is admitted as a replacement body",
                    "capture-local replacement bodies are admitted",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C079 accepted U069 G2 rewrite contract missing", result.stderr)

    def test_rejects_c076_stale_blanket_generic_rejection(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8")
                + "\nAll generic existing methods remain unsupported by ExistingClassUntypedRewrite.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C076 stale accepted U068 G1 rejection", result.stderr)

    def test_rejects_c079_stale_blanket_g2_rejection(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8")
                + "\nG2 locally Type-dependent generic methods remain unsupported by ExistingClassUntypedRewrite.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C079 stale accepted U069 G2 rejection", result.stderr)

    def test_rejects_c079_g2_type_body_or_option_overclaim(self) -> None:
        claims = (
            "G2 dependent parameter and result Type replacement is supported by ExistingClassUntypedRewrite.",
            "G2 arbitrary replacement bodies are supported by ExistingClassUntypedRewrite.",
            "G2 Option[String] method-Type replacement is supported by ExistingClassUntypedRewrite.",
        )
        for claim in claims:
            with self.subTest(claim=claim), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.make_fixture(root)
                readme = root / "README.md"
                readme.write_text(
                    readme.read_text(encoding="utf-8") + f"\n{claim}\n",
                    encoding="utf-8",
                )

                result = self.run_checker(root)

                self.assertEqual(result.returncode, 1)
                self.assertIn("C079 G2 adjacent-family overclaim", result.stderr)

    def test_rejects_missing_c079_u070_strict_val_initialization_warning(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8").replace(
                    "Generated strict `val` initializers must be appended in dependency order; ordinary Typer and TASTy success with no warnings does not make a forward eager reference safe, and the JVM may observe the default value. ",
                    "",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C079 accepted U070 strict-val warning missing", result.stderr)

    def test_rejects_stale_c070_cross_member_omit_append_exclusion(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8")
                + "\nThis behavior does not authorize cross-member composition with omission or append.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C070 stale omit/append exclusion", result.stderr)

    def test_rejects_missing_c070_bounded_owner_transaction_contract(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8").replace(
                    "Both legal immutable plan-building orders are supported",
                    "Only one plan-building order is described",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C070 bounded owner transaction contract missing", result.stderr)

    def test_rejects_c070_universal_owner_transaction_overclaim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8")
                + "\nArbitrary multiple methods and general transactional rollback across later compiler phases are supported.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C070 existing-owner overclaim", result.stderr)

    def test_rejects_stale_c065_calling_mode_warning(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            (root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md").write_text(
                "A captured by-name parameter can be inspected\n"
                "Do not call `replaceParameterType` for a captured by-name parameter\n"
                "change evaluation from by-name to strict\n"
                "not fail-closed for this case\n"
                "`UNSUPPORTED_STRUCTURE` result\n"
                "does not automatically protect whole-slot parameter-Type replacement\n"
                "Captured repeated/vararg parameters\n"
                "Do not call `replaceParameterType` for a captured repeated parameter\n"
                "multi-argument calls fail Typer\n"
                "This is also not fail-closed\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C068 stale calling-mode safety warning", result.stderr)

    def test_rejects_general_calling_mode_rewrite_overclaim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            (root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md").write_text(
                "By-name parameter-Type replacement preserves a fresh outer wrapper.\n"
                "Repeated parameter-Type replacement preserves the original star marker.\n"
                "arbitrary calling-mode rewriting is supported\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C068 calling-mode overclaim", result.stderr)

    def test_rejects_missing_c065_dynamic_tqq_layout(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8").replace(
                    "tqq\"$constructor[Int, ..$tail]\"",
                    "dynamic layout omitted",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C065 dynamic tqq documentation missing layout", result.stderr)

    def test_rejects_missing_c068_by_name_wrapper_preservation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            (root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md").write_text(
                "Repeated parameter-Type replacement preserves a fresh `PostfixOp` wrapper\n"
                "original `Ident(*)` marker by identity\n"
                "one ordinary clause of one or two parameters\n"
                "across two distinct admitted methods of the same captured class\n"
                "preserving independent method/parameter refs\n"
                "does not authorize cross-member composition with omission or append\n"
                "does not authorize arbitrary calling-mode rewriting\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn(
                "C068 bounded calling-mode safety contract missing: "
                "By-name parameter-Type replacement",
                result.stderr,
            )

    def test_rejects_missing_c068_repeated_wrapper_preservation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            (root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md").write_text(
                "By-name parameter-Type replacement preserves a fresh `ByNameTypeTree` wrapper\n"
                "one ordinary clause of one or two parameters\n"
                "across two distinct admitted methods of the same captured class\n"
                "preserving independent method/parameter refs\n"
                "does not authorize cross-member composition with omission or append\n"
                "does not authorize arbitrary calling-mode rewriting\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn(
                "C068 bounded calling-mode safety contract missing: "
                "Repeated parameter-Type replacement",
                result.stderr,
            )

    def test_rejects_stale_c065_active_inventory(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            (root / "docs/API_COMPATIBILITY_REVIEW.md").write_text(
                "current standard inventory is 796 rows / 777 groups\n"
                "hybrid inventory remains 45 rows / 43 groups\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C065 active standard API inventory", result.stderr)

    def test_rejects_stale_standard_two_selected_name_rejection(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8").replace(
                    "Standard `qq` also admits `qq\"$receiver.$first.$second\"` and "
                    "`qq\"$receiver.$first.$second($argument)\"`.",
                    "Multiple selected names remain unsupported on standard `qq`.",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C082 stale standard two-selected-name rejection", result.stderr)

    def test_rejects_stale_typed_scalameta_two_name_pending_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8").replace(
                    "Opt-in typed-Scalameta `qq` admits the same exact two-name direct/unary contract.",
                    "Opt-in typed-Scalameta two-name formal acceptance is pending.",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C082 stale typed-Scalameta pending claim", result.stderr)

    def test_rejects_missing_two_name_source_position_boundary(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                limitations.read_text(encoding="utf-8").replace(
                    "Exact two-name and direct N=3..8 candidates require recoverable original source; legacy scalar, one-name, ranked, mixed, and local-method candidates bypass source-position inspection. A new-family candidate without source fails closed.",
                    "Two-name source handling follows the ordinary matcher.",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C082 two-name source-position contract missing", result.stderr)

    def test_rejects_repeated_binder_or_marker_shipping_overclaim(self) -> None:
        overclaims = (
            'Native `qq"$receiver.$same.$same"` is supported.',
            "The `#repeat` marker is public syntax.",
            "Use the public `qq2` interpolator for two names.",
        )
        for overclaim in overclaims:
            with self.subTest(overclaim=overclaim), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.make_fixture(root)
                limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
                limitations.write_text(
                    limitations.read_text(encoding="utf-8") + "\n" + overclaim + "\n",
                    encoding="utf-8",
                )

                result = self.run_checker(root)

                self.assertEqual(result.returncode, 1)
                self.assertIn("C082 repeated-name or arity-family overclaim", result.stderr)

    def test_rejects_missing_c083_canonical_resolved_option_recipe(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            rewrite = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
            rewrite.write_text(
                rewrite.read_text(encoding="utf-8").replace(
                    "`ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)`, "
                    "`ResolvedTypeNameId`, `TypeNormalForm.STypeResolved`, and "
                    "`TypeNormalForm.STypeApply`.",
                    "public resolved constructors.",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C083 canonical Option public construction recipe missing", result.stderr)

    def test_rejects_c083_stale_blanket_applied_method_type_rejection(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8")
                + "\nAll STypeApply existing method edits are unsupported.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C083 stale blanket applied method-Type rejection", result.stderr)

    def test_rejects_c083_option_adjacent_family_overclaims(self) -> None:
        overclaims = (
            "Bare Option and String aliases are canonical global identities for existing method edits.",
            "Nested Option and other applied constructors are supported by ExistingClassUntypedRewrite.",
            "G2 dependent Option Type replacement is supported by ExistingClassUntypedRewrite.",
            "General @addOption expression wrapping is supported by ExistingClassUntypedRewrite.",
        )
        for overclaim in overclaims:
            with self.subTest(overclaim=overclaim), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.make_fixture(root)
                readme = root / "README.md"
                readme.write_text(
                    readme.read_text(encoding="utf-8") + "\n" + overclaim + "\n",
                    encoding="utf-8",
                )

                result = self.run_checker(root)

                self.assertEqual(result.returncode, 1)
                self.assertIn("C083 canonical Option adjacent-family overclaim", result.stderr)

    def test_rejects_missing_c083_cross_surface_boundary(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            matrix = root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
            matrix.write_text(
                "\n".join(
                    line
                    for line in matrix.read_text(encoding="utf-8").splitlines()
                    if not line.startswith(
                        "| Existing-class canonical resolved Option[String] method Type write |"
                    )
                )
                + "\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("C083 canonical Option cross-surface row missing", result.stderr)


    def test_rejects_stale_direct_chain_limits_on_either_frontend(self):
        for claim in ("More than two selected names remain unsupported.", "Hybrid direct N3..8 is not yet supported."):
            with self.subTest(claim=claim), tempfile.TemporaryDirectory() as tmp:
                root=Path(tmp);self.make_fixture(root)
                p=root/"docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
                p.write_text(p.read_text()+"\n"+claim+"\n")
                result=self.run_checker(root)
                self.assertEqual(1,result.returncode,result.stderr)
                self.assertIn("direct selected-chain boundary",result.stderr)

    def test_requires_both_frontend_direct_depth_contract(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);self.make_fixture(root)
            p=root/"docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            p.write_text(p.read_text().replace("Both typed frontends support direct flat selected-member chains at depths N=1..8.","The previous smaller boundary remains."))
            result=self.run_checker(root)
            self.assertEqual(1,result.returncode,result.stderr)
            self.assertIn("direct selected-chain boundary",result.stderr)


    def test_rejects_wrapped_or_linked_stale_selected_depth(self):
        cases = (("docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md", "More than two\nselected names remain unsupported."), ("docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md", "More than two selected names remain unsupported."), ("README.md", "At most two selected names are supported."))
        for path, claim in cases:
            with self.subTest(path=path), tempfile.TemporaryDirectory() as tmp:
                root=Path(tmp);self.make_fixture(root);p=root/path
                p.write_text(p.read_text()+"\n"+claim+"\n")
                result=self.run_checker(root)
                self.assertEqual(1,result.returncode,result.stderr)
                self.assertIn("direct selected-chain boundary",result.stderr)

if __name__ == "__main__":
    unittest.main()
