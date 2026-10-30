# Rooted quoted existing-method rewrites

The exact Scala-cross Dotty backend in source builds of `0.4.0-SNAPSHOT`
provides `quasiquotes.definitions.dotty.ExistingMethodQuotedRewrite` alongside
`ExistingClassUntypedRewrite`. This is an experimental source-built producer;
it is not part of the published `0.3.0` release. Use the backend artifact built
for the exact Scala compiler version of the consumer.

A selection couples a ScalaMeta method authoring view to its original class
capture and member reference. Match `Selection.projected` with ordinary
`scala.meta.q`, then pass the quoted replacement to `Selection.replace` with
a plan from that same capture. Apply the returned plan once through
`ExistingClassUntypedRewrite(capture, plan)`.

```scala
import dotty.tools.dotc.core.Contexts.Context
import quasiquotes.definitions.dotty.{ExistingClassUntypedRewrite, ExistingMethodQuotedRewrite}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import scala.meta.*

def echoPlan(capture: Capture, member: MemberRef)(using Context): Either[Failure, EditPlan] =
  for
    selection <- ExistingMethodQuotedRewrite.select(capture, member)
    replacement <- selection.projected match
      case q"def echo($value: AnyVal): AnyVal = $body" => value match
        case name: Term.Name =>
          Right(q"def echo($name: Int): Int = _root_.java.lang.Math.abs($name)")
        case _ => Left(Failure("NAME_REQUIRED", "a term parameter name is required"))
      case _ => Left(Failure("METHOD_REQUIRED", "the echo method did not match"))
    plan <- selection.replace(capture.emptyPlan, replacement)
  yield plan

// The caller uses the original capture and calls the public owner transaction once:
// echoPlan(capture, member).flatMap(plan => ExistingClassUntypedRewrite(capture, plan))
```

The explicit `Term.Name` narrowing is required by the current ScalaMeta splice
syntax. The analogous `magnitude(number)` pattern can produce
`_root_.java.lang.StrictMath.abs(number)`. The replacement quote drives the
parameter/result edits and body; the original method and untouched siblings
retain their original identity or reconstruction sites as appropriate.

The initial bound is one concrete, nongeneric, strict ordinary unary method
whose original body is its own parameter reference and whose simple input
Types are admitted by the existing backend. Replacement declarations preserve
the method and parameter names, use `Int` parameter/result Types, and call one
of the two explicit rooted Java `abs` targets with the output declaration's own
parameter. Hidden generic Type clauses, contextual/default/by-name/repeated or
curried parameters, arbitrary bodies, old-body wrapping and replacement
modifiers are unsupported. Original admitted method modifiers are preserved.
Duplicate direct method names and a second quoted selection in one plan are
rejected in this first slice.

Bare `Math.abs` and `StrictMath.abs` are rejected by this canonical-target
facade. They can resolve to source-local declarations under shadowing; the
facade does not silently qualify them. Existing low-level `replaceBody`
lexical behavior is unchanged. Ordinary Typer resolves the rooted target;
the producer creates no declaration Symbols before Typer.

Plans are immutable. Existing disjoint edits survive composition; conflicting
Type/body edits or omission fail. Selection, plan and parameter authority are
owned by the original capture and compiler Context. Re-capturing the same raw
owner creates different authority. Changed source bytes/sites/topology or
foreign references are rejected. The ScalaMeta view is an authoring view,
not an exact whole-owner roundtrip or a source certificate usable elsewhere.
The compiler's raw trees remain mutable, so callers must retain the original
capture and avoid changing its graph or source between selection and apply.

This producer does not own annotation discovery, handler scheduling or
whole-compilation rollback. A consumer using an annotation framework must
qualify that framework's lifecycle and classloader separately. No release or
handler integration is implied by source-level producer availability.
