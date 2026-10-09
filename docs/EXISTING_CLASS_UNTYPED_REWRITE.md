# Existing-class untyped rewrite

`quasiquotes.definitions.dotty.ExistingClassUntypedRewrite` is the public,
exact-compiler-version facade for one bounded pre-Typer class transaction. It is
present in the active `0.4.0-SNAPSHOT` source tree; it is not part of the
released `0.3.0` artifacts. Consumers must use the `dottyInternal` artifact
built for the exact active Scala compiler line and call it while the class is
still an untyped, symbol-free compiler tree.

The operation is:

```text
capture existing class
  -> inspect ordered direct members and admitted method slots
  -> build an immutable capture-local EditPlan
  -> apply once
  -> receive one rebuilt class and ordered direct-member identities
```

It is not an arbitrary Dotty AST editor. It does not select a compiler phase,
find an annotation target, insert the result into a compilation unit, run
Typer, repair owners or symbols, or roll back a surrounding plugin operation.
Macro-Paradise retains annotation lifecycle and placement around this one-class
transaction. No public `u*` quasiquote syntax is selected.

## Capture and selection

`capture(existingClass)` delegates to the exact existing-class capture
authority and records the ordered direct-member topology. `Capture.members`
contains public read-only views in that order. A view has its direct-member
index, diagnostic name, opaque `MemberRef`, `ExactIdentity`, and a
`MemberKind` whose documented codes are `METHOD`, `VALUE`, and
`TYPE_MEMBER`. Other raw children remain opaque; their presence is not
reinterpreted.

The index is topology authority, not a name lookup. Call `member(index)`, then
pass its capture-local ref to `method`. Refs have no public constructors and
cannot be mixed across captures. Names are diagnostic only, so overloads do
not weaken selection.

`ExactIdentity.sameObjectAs` answers only whether two handles wrap the same
raw JVM object. It is deliberately different from structural equality: a
fresh structurally equal tree is not the same object.

## Method views and bounded semantic slots

The admitted ordinary method topology is exactly one ordinary parameter clause
containing one or two parameters. Other methods remain visible as members but
`method` fails closed with `UNSUPPORTED_STRUCTURE`. A successful
`MethodView` exposes:

- ordered parameter clauses and parameters;
- exact identities for each raw parameter, declared Type, result Type, and RHS;
- bounded `TypeNormalForm` projections for declared/result Types;
- a bounded `TermShape` projection for the body; and
- an `ExistingMethodParameterScope` for binder-safe body construction.

Raw capture and semantic projection are independent. A Type or body slot may
return `UNSUPPORTED_STRUCTURE` while its exact identity remains available.
The parameter scope is constructed from the exact method topology, not from
body projection, so `binder` and `reference` remain usable when
`body.semantic` is unsupported. Repeated lookups are stable. A parameter ref
from another method or capture is rejected.

## Immutable edit plans

`Capture.emptyPlan` is immutable. Each successful operation returns another
plan:

- `omit(member)`;
- `replaceParameterType(parameter, TypeNormalForm)`;
- `replaceResultType(method, TypeNormalForm)`;
- `replaceBody(method, TermShape)`; and
- `append(SemanticDefinition, virtualSourceName)`.

Duplicate field edits, duplicate omission, and omission plus an edit of the
same member fail with `EDIT_CONFLICT`. Foreign refs, null fragments, foreign
binder graphs, and invalid generated-source names fail before the owner
transaction. Supported replacement bodies must use references obtained from
the selected method's public parameter scope; matching display text does not
substitute for binder identity.

Append accepts the public semantic Definition families admitted by
`DefinitionGeneratedOriginLowering`, including a simple Type alias. Multiple
appends retain plan order. Every appended member keeps its generated virtual
source.

### Current by-name and repeated-parameter restrictions

A captured by-name parameter can be inspected, and its containing method can
receive a binder-aware body replacement or result-Type replacement. Do not call
`replaceParameterType` for a captured by-name parameter in the active
0.4.0-SNAPSHOT implementation: rebuilding that whole parameter Type can erase
the by-name wrapper and change evaluation from by-name to strict.

This is a caller usage restriction, not a new failure mode. The current API is
not fail-closed for this case, and an `UNSUPPORTED_STRUCTURE` result from the
bounded semantic Type projection does not automatically protect whole-slot
parameter-Type replacement. Keep the original parameter Type until a separately
proven correction is integrated.

Captured repeated/vararg parameters have the same current usage restriction.
Their exact raw Type retains the outer `PostfixOp(elementType, Ident(*))`
wrapper, and body-only or result-Type-only edits preserve it. Do not call
`replaceParameterType` for a captured repeated parameter: replacing the whole
slot with a scalar Type erases that wrapper and makes previously valid
multi-argument calls fail Typer. This is also not fail-closed, and semantic
projection rejection does not guard the separate whole-slot replacement.

## One atomic owner transaction

Calling `ExistingClassUntypedRewrite(capture, plan)` prepares all requested
method-Type/body replacements and generated definitions, then delegates one
final owner transaction. It does not publish intermediate owners.

For a no-op plan, `changed == false` and `result.tree` is the exact original
class object. A changed plan returns one fresh class/Template shell. Untouched
direct members remain the exact original objects, replaced methods are fresh
only at their admitted sites, omitted members disappear, and appended members
appear last in plan order. Applying the same changed plan again creates a
fresh owner shell again; it does not mutate the captured graph.

`Result.directMemberIdentities` reports final direct-member order and lets a
caller compare preserved children through `sameObjectAs`.

The three reference compositions are:

| Case | Public plan |
| --- | --- |
| U1 | preserve existing members and append one generated sibling |
| U2 | for a strict parameter, replace one admitted parameter Type, result Type, and binder-aware body |
| U3 | perform that strict-parameter U2 plan and append generated siblings in the same apply call |

Omission can be composed with nonconflicting method rewrites and appends in the
same transaction.

## Compiled first use

The following U3-shaped example is compiled in the external-consumer test
package. It assumes direct member 0 is an admitted one- or two-parameter
ordinary method with a strict parameter; it does not exercise the unsafe
by-name parameter-Type edit. Production callers should branch on each `Either`
and inspect the returned topology rather than infer it from a name.

<!-- snippet:existing-class-untyped-rewrite-first-use:start -->
```scala
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.definitions.{DefinitionName, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Failure, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

object ExistingClassUntypedRewriteFirstUse:
  private val IntType = TypeNormalForm.STypeIdent("Int")
  private val GeneratedSibling =
    val name = DefinitionName.fromSource("bonus")
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    SemanticDefinition.concreteMethod(name, Vector.empty, IntType)(
      _ => Right(TermShape.Literal("5"))
    ).fold(error => throw new IllegalArgumentException(error.message), identity)

  def rewrite(existingClass: untpd.Tree)(using Context): Either[Failure, Result] =
    for
      captured <- ExistingClassUntypedRewrite.capture(existingClass)
      member <- captured.member(0)
      method <- captured.method(member.ref)
      parameter = method.parameterClauses.head.parameters.head
      reference <- method.parameterScope.reference(parameter.ref)
      body = TermShape.Apply(
        TermShape.Select(TermShape.Identifier("Math", false), "abs"),
        List(reference)
      )
      p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, IntType)
      p2 <- p1.replaceResultType(method.ref, IntType)
      p3 <- p2.replaceBody(method.ref, body)
      p4 <- p3.append(GeneratedSibling, "generated/ExistingClassBonus.scala")
      result <- ExistingClassUntypedRewrite(captured, p4)
    yield result
```
<!-- snippet:existing-class-untyped-rewrite-first-use:end -->

For U1, omit the three replace calls and append from `emptyPlan`. For U2,
apply `p3` without the append. In all three cases, invoke the facade exactly
once for the finished plan.

## Failures and limits

Branch on `Failure.code`; `detail` is diagnostic and may name the delegated
private stage. The stable public categories are `MISSING_INPUT`,
`CAPTURE_FAILED`, `SELECTION_FAILED`, `UNSUPPORTED_STRUCTURE`,
`SEMANTIC_FRAGMENT_INVALID`, `EDIT_CONFLICT`, `LOWERING_FAILED`,
`ORIGIN_FAILED`, `RECONSTRUCTION_FAILED`, and
`INTERNAL_INVARIANT_FAILED`.

Unsupported method topology, unsupported semantic slots, and unsupported
replacement grammar fail closed. The facade does not claim arbitrary method
shapes, arbitrary Terms or Types, arbitrary member positioning, class
authoring, post-Typer rewriting, symbol transport, owner repair, or
cross-version compiler compatibility. Its raw input and raw result are the
only public raw-tree boundary; the validated refs, identities, views, and plans
remain opaque.
