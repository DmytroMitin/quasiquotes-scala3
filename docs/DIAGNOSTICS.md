# Diagnostics

The compiler-free and compiler-coupled entry points deliberately preserve
their existing failure types.

## Core failures

The bounded contextual-method, single-parameter-method, and exact-two-parameter
method APIs return `Either[PublicFailure, A]`.
`PublicFailure.code` is the experimental machine-readable identity, `message`
is presentation text, and `anchor` identifies the affected component when one
exists.

```scala
val show = CompletedType.named("Show").toOption.get
val failure = CompletedType.applied(show, Vector.empty).left.toOption.get

assert(failure.code == "invalid-type-application")
assert(failure.anchor.exists(_.componentCode == "type-application"))
assert(failure.message == "A type application requires at least one argument.")
```

Other first-use codes include `invalid-name`, `undeclared-type-parameter`, and
`invalid-contextual-method-contract`.

`DefinitionConstruction.singleParameterMethod` adds one stable code:
`invalid-single-parameter-method-contract`. Its anchors distinguish
`parameter-name`, `parameter-type`, `result-type`, and `body`. Representative
failures include an unsupported completed type, a free reference passed where
an explicit definition-parameter reference is required, a mismatched reference
name, and unequal parameter/result types for the identity body. Messages remain
presentation text; consumers should branch on the code and anchor.

`DefinitionConstruction.twoParameterMethod` adds
`invalid-two-parameter-method-contract` for duplicate declared names,
unsupported parameter/result types, free or unknown body references, and a
result type that differs from the selected parameter. Name syntax failures
remain `invalid-name`; exact-two contract failures reuse the
`parameter-name`, `parameter-type`, `result-type`, and `body` anchors.

## Frontend located failures

Source adapters retain `TypeQuasiquoteError`. Their `Located` variants wrap it
in `LocatedDiagnostic`, which may carry either an exact occurrence or a
conservative whole-source location.

```scala
import quasiquotes.source.DiagnosticPrecision
import quasiquotes.types.{QuasiTypeConstruct, TypeNormalForm}

val failure = QuasiTypeConstruct.fromTemplateLocated(
  "Either[$left, $right]",
  "right" -> TypeNormalForm.STypeIdent("String")
).left.toOption.get

assert(failure.diagnostic.message ==
  "Missing type-construction binding `$left`.")
assert(failure.location.exists(
  _.precision == DiagnosticPrecision.ExactOccurrence
))
```

Repeated missing holes, unsupported complete shapes, and unexpected bindings
use whole-source precision when selecting one narrower span would be
misleading. Parse failures use the best structured parser span that can be
truthfully mapped; some malformed inputs have no usable span.

Representative messages are:

```text
Unsupported applied type constructor `Map`; supported constructors are List/1, Option/1, Either/2.
Expected exactly 2 type arguments for `Either`, but found 1.
Selected type constructor syntax `scala.Either[...]` is not supported; use unqualified `Either[...]` in the current experimental surface.
Type-constructor hole `$F[...]` is not supported; use one of the fixed constructors List/1, Option/1, Either/2.
```

Frontend errors remain message-centric; there is no new structured frontend
error-code algebra. Consumers should not parse arbitrary message text as a
long-term compatibility mechanism. A future additive diagnostic view can be
considered if concrete tooling needs justify one without breaking the existing
`Either` entry points.

## `qq` extractor diagnostics

The pattern extractor deliberately separates template failure from target
mismatch. An ordinary structural mismatch returns `None` and reaches the next
Scala pattern case. A malformed or unsupported template aborts the surrounding
macro expansion with a message beginning:

```text
Invalid qq term-pattern template:
```

For example, `case qq"$value +"` reports the underlying parser summary rather
than silently becoming a non-match or leaking an exception. The extractor
protocol cannot return rich mismatch details; use `QuasiPattern.termLocated`
for recoverable template diagnostics and `matchTerm` for explicit match
failures.

## Lambda1 failures

`QuasiPattern.termLocated` returns an actionable located error for unsupported
lambda syntax. The `qr` interpolator reports the same admitted boundary as a
compile-time error at the offending quasiquote source.

| Input | Remedy |
| --- | --- |
| `x => x` | Add an explicit parameter type, for example `(x: Int) => x`. |
| `(x: Int, y: Int) => x + y` | Rewrite the quoted form as one explicitly typed parameter. |
| `(x: Int) => ((y: Int) => y)` | Move the nested lambda outside the Lambda1 pattern or construction. |
| `(x: Int) ?=> x` | Use an ordinary `=>` function; context functions are outside this surface. |

Representative messages are:

```text
Lambda1 requires an explicit parameter type; write a parameter such as `(x: Int)`.
Lambda1 supports exactly one explicitly typed ordinary parameter; rewrite this as a one-parameter lambda.
Lambda1 bodies do not support nested lambdas; move the nested lambda outside this pattern.
Lambda1 supports ordinary `=>` functions only; replace `?=>` with an explicitly typed ordinary parameter.
```

A term splice inside Lambda1 is also rejected when the spliced tree contains a
local `val`, `def`, or class definition. Splice a definition-free expression
instead; this surface does not perform general owner migration. A string
literal such as `(x: Int) => "?=>"` is ordinary supported body content and is
not misclassified as a context function.
