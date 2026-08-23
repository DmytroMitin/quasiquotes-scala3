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

P2 admits exactly `{ val x: SupportedType = initializer; result }`. Diagnostics
separate a missing explicit type, `var`, `lazy val`, a pattern/destructuring
binder, multiple local values, local `def`, unsupported initializer/result
children, and an owner-sensitive splice containing definitions. The
public-default and Scalameta opt-in paths retain controlled block-family
wording. Located parsing uses the deepest truthful source span available;
generated target matching requires no source reconstruction. Broader P2/P3
forms fail closed rather than falling back to P1.

## `tqr` and `tqq` diagnostics

The reflected type syntax has two controlled macro-expansion prefixes:

```text
Invalid tqr type template:
Invalid tqq type-pattern template:
```

Malformed or unsupported templates abort during macro expansion. `tqr` also
rejects a wrong `StringContext` arity, a null context, and any splice whose
`TypeRepr` is outside the bounded inspector. It does not fall back to direct
compiler-tree construction. `tqq` treats unsupported target `TypeRepr` values
and ordinary structural mismatches as `None`, so the next Scala pattern case
runs; only the template itself is an aborting boundary.

Use `QuasiTypeConstruct.fromTemplateLocated` or
`QuasiTypePattern.patternLocated` when a recoverable structured template
diagnostic is required. The interpolator/extractor protocols intentionally
return a reflected result or Scala pattern match rather than an `Either`.

## `dqr` diagnostics

Every rejection owned by the public reflected definition interpolator begins:

```text
Invalid dqr definition template:
```

This includes malformed or excluded definition shapes, wrong splice arity,
unsupported reflected types, unequal normalized parameter/result types, a
wrong literal body binder, and hostile null or inconsistent `StringContext`
inputs. A non-`TypeRepr` interpolation argument is rejected earlier by the
Scala method signature. The interpolator aborts the surrounding macro
expansion; use the compiler-free `DefinitionConstruction.singleParameterMethod`
when a recoverable `Either[PublicFailure, ...]` is required.

## Programmatic definition matcher errors and mismatches

`DefinitionPattern.singleParameter(source)` returns
`Left(DefinitionPatternError)` for null, malformed, unsupported, or
out-of-grammar matcher source. The error's `message` is phase-neutral and does
not expose parser placeholders or compiler internals. This configuration step
does not abort macro expansion.

After configuration succeeds, `pattern.matchDefinition(target)` returns
`None` for an ordinary mismatch: different fixed names or types, unsupported
target types, wrong parameter-list shape, missing RHS, or an invalid
method/parameter symbol-owner relationship. These are normal matching results,
not `DefinitionPatternError` values and not compiler diagnostics. A successful
match returns `Some(result)` containing the original caller-owned reflected
objects.

`dqq` validates its `StringContext` before compiling the same bounded matcher
source. Null contexts or literal parts, zero or multiple interpolation slots,
malformed templates, and every out-of-grammar slot position abort macro
expansion with the stable prefix:

```text
Invalid dqq definition-pattern template:
```

The remainder is only the public `DefinitionPatternError.message`; parser,
matcher, placeholder, and compiler-internal details are not exposed. Once the
template is valid, a target mismatch is an ordinary extractor fall-through,
not a diagnostic.

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
