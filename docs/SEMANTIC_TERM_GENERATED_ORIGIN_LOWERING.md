# Semantic Term generated-origin lowering

`quasiquotes.terms.dotty.TermGeneratedOriginLowering` is an implemented public
facade in the existing exact-version Dotty-internal artifact of the unpublished
development tree. It takes public `TermShape`
and a caller-provided virtual source name under an active Dotty `Context`.
It returns `Either[Failure, Lowered]`, with a positioned `untpd.Tree`,
`generatedSource`, `sourceFile`, and `virtualSourceName` derived from that file.
Consumers cannot construct `Lowered` directly.

`TermUntypedLowering` remains the broader source-free semantic Term operation.
The new sibling shares its checked semantic/completion/exact authority once,
then applies additional source-admission rules and positions the same raw
structure. Neither operation converts its input through Scalameta or reparses
rendered text. The historical `ScalametaTermGeneratedOriginBridge` remains
separate, with its original admission, source and diagnostic order.

<!-- snippet:semantic-term-origin:start -->
```scala
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermParameterSpec, TermShapeBindings}
import quasiquotes.terms.dotty.TermGeneratedOriginLowering
import quasiquotes.types.TypeNormalForm

object SemanticTermOriginExample:
  def check(): Unit =
    given Context = new ContextBase().initialCtx
    val semantic = TermShapeBindings.lambda(
      Vector(TermParameterSpec("x", TypeNormalForm.STypeIdent("Int")))
    ) { scope =>
      scope.reference(scope.parameterBinders.head.head)
    }.fold(error => sys.error(error.message), identity)

    val result = TermGeneratedOriginLowering
      .lower(semantic, "generated/Identity.scala")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    assert(result.tree.isInstanceOf[untpd.Function])
    assert(result.generatedSource == "(x: Int) => x")
    assert(result.virtualSourceName == "generated/Identity.scala")
    assert(result.sourceFile.content.mkString == result.generatedSource)
    assert(result.tree.source eq result.sourceFile)
    assert(result.tree.span.start == 0)
    assert(result.tree.span.end == result.generatedSource.length)
```
<!-- snippet:semantic-term-origin:end -->

## Current admission

All current source-free semantic completion, binder-scope, sidecar, exact-tree
and topology limits still apply. Successful families include supported decimal,
Boolean and semantic String literals; identifiers/selections; ordinary Apply;
bounded infix/unary; tuples of arity 2 through 22; if/else; standard s
interpolation; qualified one-list construction; explicit parentheses; expression
blocks; primitive ascription; and the admitted single-parameter lambda,
local-value and local-method families.

The generated-source sibling deliberately accepts less than source-free
lowering:

- Decoded identifier, selected member and binder names must match
  `[A-Za-z_][A-Za-z0-9_]*` and cannot be `_`. Existing supported keyword names
  are backtick-encoded. Symbolic, dollar-bearing, Unicode, embedded-dot,
  whitespace and already-backticked semantic names are outside this bound.
- Constructor paths retain at least two plain identifier segments. Each segment
  additionally excludes `_` and the existing source encoder's keyword set.
- A signed decimal literal directly used as a Select qualifier needs an
  explicit semantic `Parenthesized` wrapper: `(-1).abs` is admitted;
  `Select(Literal("-1"), "abs")` is rejected. This uniform rule avoids the
  different treatment of ungrouped signed receivers in the supported compilers.

These are conservative source-language choices, not limits of all Scala syntax.
Ordinary signed literals and Apply functions are not excluded by the receiver
rule. Argument counts live in the semantic structure; direct nested Apply still
follows the source-free facade's current rejection, while explicitly wrapped
functions follow its existing structural policy.

Public binder builders supply the existing primitive-type envelope. Actual
frontend-produced root local-method shapes can additionally complete supported
recursive List/Option/Either, Tuple2/3 and Function1/2 parameter/result Types.
Wrapping those rich roots may fail current completion and is not repaired here.
Private explicit sidecar support does not imply public rich ascription/lambda/
local-value support. A local method's block result may be any admitted term in
its method-binder scope, not only a method reference.

## Origin, identity and failures

Repeated calls with the same semantic input, display names, path and exact
compiler line produce deterministic text/topology/spans but fresh result,
SourceFile and every material tree node. Each material node uses the returned
SourceFile by identity, carries a valid contained span, has NoSymbol, and
contains no TypedSplice. Root coverage is [0, generatedSource.length]; the
point comes from its structural plan and need not be zero. Offsets are UTF-16
String offsets. EmptyTree placeholders are non-material.

Binder association follows the original semantic graph throughout lowering.
Raw references have NoSymbol; bound and free same-spelling names are not made
symbolically distinct. Alpha-equivalent renamed inputs may have different
source text and spans. This is not an additional hygiene or name-resolution
service.

Branch on `Failure.code`; `detail` is diagnostic. Exactly seven categories are
exposed: `MISSING_INPUT`, `INVALID_VIRTUAL_SOURCE`,
`MALFORMED_SEMANTIC_VALUE`, `UNSUPPORTED_SEMANTIC_VALUE`,
`EXACT_LOWERING_FAILED`, `GENERATED_ORIGIN_FAILED`, and
`INTERNAL_INVARIANT_FAILED`.

Validation checks absent Term, absent path, invalid non-null path, then the
shared source-free structure/completion/admission/exact/topology stages, then
source-role/grouping admission, planning, positioning and final origin checks.
An unsupported literal therefore fails at exact lowering before a later
source-name error. An invalid path wins over semantic failures.

Null path is missing input; empty/blank, surrounding trim whitespace, NUL/CR/LF
and names whose represented virtual path differs are invalid. Existing
internal spaces and relative/virtual forms remain allowed by the shared path
authority. There is no file write, existence check, canonicalization or required
extension. The caller owns insertion and ordinary compiler lifecycle/typing.
No public existing-owner transaction or Type generated-origin facade is added.

Use an artifact built for the exact active compiler version: 3.3.8, 3.8.4 or
3.9.0. Source availability and local qualification do not imply a remotely
released artifact or release authorization.
