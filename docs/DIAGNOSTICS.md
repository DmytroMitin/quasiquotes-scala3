# Diagnostics

The compiler-free and compiler-coupled entry points deliberately preserve
their existing failure types.

## Core failures

The bounded contextual-method API returns `Either[PublicFailure, A]`.
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
