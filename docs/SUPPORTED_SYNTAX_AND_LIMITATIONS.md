# Supported syntax and limitations

The implementation is a structural research subset, not a complete Scala 3
quasiquote system.

For a compact phase-neutral overview across terms, types, and definitions, see
the [syntax support matrix](SYNTAX_SUPPORT_MATRIX.md). This document provides
the detailed semantic and diagnostic caveats behind that table.

Currently exercised areas include:

- identifiers, selections, applications, typed terms, binder-free P1 blocks,
  one explicitly typed eager immutable local-val P2 block, tuples, unary
  operations, and standard string interpolation in bounded structural forms;
- ordinary lambdas with exactly one explicitly typed parameter, with scoped
  binder identity, alpha-aware construction/matching, and complete-body holes;
- compiler-free public single- and exact-two-parameter method constructors,
  plus richer package-private definition shapes with scoped references and
  alpha-aware template/completion semantics;
- parser/shape inspection for type identifiers, selections, applications,
  tuples, functions, wildcards, unions/intersections, annotations, refinements,
  and selected bounds/match forms; public construction and matching remain the
  narrower families in the syntax matrix;
- term and type holes with collision-safe rewriting and repeated-hole checks;
- construction-only `SelectedMemberName` holes in the exact name field of an
  explicit receiver selection, restored structurally from a dedicated
  collision-safe placeholder;
- compiler-free term/type/definition templates and completed values;
- bounded public contextual, single-ordinary-parameter, and exact-two-parameter
  construction contracts;
- source spans, diagnostic anchors, and exact-version lowering adapters;
- recursively nested `List` and `Option` types plus binary `Either`, with
  structural argument order, repeated type holes, construction, quoted
  lowering/inspection, typed ascriptions, and scoped `Type[t]` evidence.

Important limitations:

- no claim of complete Scala grammar or compiler-tree coverage;
- no stable raw-tree public API;
- no general owner/symbol repair or arbitrary generated-definition placement;
- Lambda1 excludes inferred or multiple parameters, nested/context/pattern
  lambdas, binder-name holes, and local definitions;
- P1 blocks admit only one or more ordered expression prefixes plus a final
  result; P2 admits exactly one simple explicitly typed eager immutable local
  `val`; the whole tree admits at most one P2 binder and rejects P2/Lambda1
  same-name source shadowing; inferred, mutable, lazy, pattern,
  multiple/recursive values,
  local `def` (P3), imports, other definitions, and unrelated statement
  families remain excluded;
- compiler-internal behavior is exact-version-sensitive;
- public definition construction is intentionally narrow;
- public `dqr` has no recoverable source carrier, parameter-span projection, or
  arithmetic/literal/general-expression body builder; exact-two syntax and the
  exact internal definition backends remain package-private;
- interpolation and type support expands incrementally, so unsupported shapes
  return explicit errors rather than falling back to unchecked trees;
- dynamic selected-member names are decoded semantic values, not source
  spellings or symbols. The conservative factory admits plain ASCII
  identifiers, symbolic ASCII operators, and single-space-separated safe ASCII
  words. It rejects empty/null, `$`, backticks, controls, dots,
  compiler-special names, Unicode, and other grammar. A wrapper is legal only
  after an explicit receiver dot; bare, constructor, type, definition, import,
  pattern, and dynamic infix positions fail closed. Selection uses
  `Select.unique`; missing/inaccessible and overloaded members are rejected,
  and an explicit following empty or nonempty argument list keeps the selected
  method callable until that source `Apply` is lowered. A fixed selection with
  no explicit argument list retains its existing value-position normalization;
- public `qq` extractor templates require at least one interpolated term slot;
  slots are distinct and ordered, with no type/sequence/backreference syntax;
  they do not capture or accept dynamic selected-member names;
- public `tqr` and `tqq` type templates use zero or more distinct ordinal
  whole-type slots; they do not admit constructor, higher-kinded, wildcard,
  sequence, binder-name, or mixed-category slots;
- ordinary quoted standard-`s` interpolation has a bounded exact internal
  backend with canonical escaping and generated-origin spans; `raw`, `f`,
  custom interpolators, and triple-quoted `s` remain unsupported;
- APIs and rendered forms remain experimental and may change;
- applied constructor admission is fixed to `List` (arity 1), `Option`
  (arity 1), and `Either` (arity 2); the experimental explicit
  `GlobalSelectedTypeEnvironment`/`GlobalSelectedTypeFrontend` surface also
  admits their canonical selected declarations by full resolved identity;
- canonical globally addressable selected terminals are experimental and
  programmatic only. Arbitrary selected constructors, arbitrary labels,
  alternate/import-shortened spellings, stable-term paths, local owners,
  constructor holes, higher-kinded types, aliases, semantic name resolution,
  subtyping, and compiler equality are not supported. Ordinary `tqr`/`tqq`
  behavior is unchanged.

## Bounded term-pattern extractor

Inside an active macro `Quotes`, import `quasiquotes.matching.QuasiPattern.*`
and match a caller-owned reflected term with syntax such as:

```scala
expression.asTerm match
  case qq"$left + $right" =>
    // left and right are q.reflect.Term values in source-slot order
  case _ =>
```

The extractor synthesizes collision-safe semantic hole IDs by ordinal and
projects the existing matcher's bindings back to the Scala pattern in
left-to-right order. The Scala binder spellings `left` and `right` are not
semantic hole names. Every slot is distinct, even if source binder spellings
would otherwise suggest equality. Existing explicit
`QuasiPattern.term("$x + $x")` repeated-hole equality is unchanged.

This first surface admits term captures only and requires at least one slot. It
has no type or mixed-category holes, sequence/splice holes, binder-name holes,
backreferences, definitions, or type-pattern syntax. Ordinary structural
mismatch returns `None` through pattern fallthrough. A malformed or unsupported
template reports `Invalid qq term-pattern template: ...` during macro
expansion; use `termLocated` when a recoverable structured diagnostic is needed.

Captured terms retain the caller's active `q.reflect.Term` path and original
compiler ownership. They are not detached portable trees and the extractor
does not create another `Quotes` universe.

## Binder-free P1 blocks

The admitted block form preserves an ordered nonempty prefix of already
supported Term expressions and a distinct final result:

```scala
val built = qr"{ $first; consume($second); $result }"

target match
  case qq"{ $first; consume($second); $result }" =>
    // captures are the original reflected children, in source order
  case _ =>
```

Single-expression braces are P0 wrappers and collapse to the enclosed Term;
they are not represented as a P1 block. Every P1 child must already be
supported outside a block, so the block surface does not implicitly admit
assignment, `return`, `throw`, `try`, loops, match/case, for-comprehensions, or
other unsupported families.

Programmatic repeated names retain the existing structural-equality rule, and
successful matches preserve the exact original reflected subtrees even for
generated/source-poor targets. Binder-bearing blocks are classified separately
as P2/P3. P1 itself introduces no binder, owner repair,
alpha-equivalence, or capture-avoidance semantics beyond those already owned
by admitted child forms.

## Single typed local immutable val (P2)

The only binder-bearing block admitted by the public Term surface is:

```scala
{ val x: Int = initializer; resultUsing(x) }
```

The binder is a simple literal identifier and the type annotation is mandatory
and must belong to the existing admitted Type subset. The initializer is
inspected and lowered in the enclosing scope. A fresh project `BinderId` and a
fresh reflected local symbol are introduced only for the final result. Binder
display spelling is therefore irrelevant to alpha equality, while an external
spliced tree whose printed name is also `x` retains its original symbol and is
not captured.

`qr` builds a real `ValDef` owned by the active `Symbol.spliceOwner`; result
references use that exact local symbol. `qq` reconstructs the relationship from
target symbols and can match `{ val x: Int = $init; x }` against an equivalent
target using a different binder spelling. Ordinary captures return their exact
original reflected subtrees. A splice containing owned definitions is rejected
because this tranche does not migrate ownership.

There is no inferred type, `var`, `lazy val`, pattern/destructuring binder,
second or nested P2 local value anywhere in the quasiquote tree, P2/Lambda1
same-name source shadowing, recursion/self-reference, binder-name hole, or
local method support. A distinct-name Lambda1 may coexist with the single P2
binder. Same-text external interpolation is not source-binder shadowing and
retains its caller-owned symbol. The unpublished exact untyped backend also remains
closed to this node; it fails with its existing `Block` boundary rather than
claiming owner-free raw-tree support.

## Bounded reflected type interpolators

Inside an active macro `Quotes`, import `quasiquotes.types.QuasiTypequotes.*`.
Construction accepts caller-owned reflected types and returns a reflected type
in the same Quotes path:

```scala
val element: q.reflect.TypeRepr = q.reflect.TypeRepr.of[String]
val result: q.reflect.TypeRepr = tqr"Either[Int, List[$element]]"
```

Every interpolation argument is inspected into the existing bounded
`TypeNormalForm`, the existing `TypeTemplate` construction policy is applied,
and the completed neutral form is lowered with the existing public type
lowerer. No direct raw compiler-tree construction or fallback path is used.
Zero, one, and multiple whole-type slots are supported.

Matching uses the same bounded inspector and authoritative `TypePattern`
semantics:

```scala
target match
  case tqq"Either[$left, $right]" =>
    // left and right are the original q.reflect.TypeRepr target subtrees
  case _ =>
```

The inspector records internal structural paths while normalizing, and the
extractor projects successful hole paths back to the exact original target
subtrees. Captures are therefore caller-owned and may carry compiler-local
identity; they are not reconstructed portable types. Unsupported targets and
ordinary structural mismatches fall through. A malformed or unsupported
template aborts with `Invalid tqq type-pattern template: ...`; an unsupported
construction template or splice aborts with `Invalid tqr type template: ...`.

Interpolated slots are collision-safe ordinal positions and are always
distinct, independent of Scala binder spelling. Named and repeated holes are
still available through `QuasiTypequotes.tqq(source)` and retain structural
equality semantics. Constructor holes and general type-constructor resolution
remain outside both public interpolated forms.

## Binder-aware Lambda1

The admitted lambda form is exactly one ordinary explicitly typed parameter:

```scala
(x: Int) => x
(x: Int) => x + 1
(x: Int) => f(x)
(x: Int) => if x > 0 then x else 0
```

The parameter declaration, references bound by it, and free references are
distinct structural roles. Consequently `(x: Int) => x` and `(y: Int) => y`
compare alpha-equally, while `(x: Int) => freeX` does not compare equal to an
identity lambda merely because `freeX` has similar text. Parameter type stays
part of structural equality.

`qr` builds a genuine quoted lambda. A spliced external term retains its
original resolved identity and is not captured when its source name matches
the new parameter display name. Splices containing local `val`, `def`, or
class definitions are rejected because this surface does not provide general
owner migration.

Pattern body holes return the original reflected target term, including its
compiler symbols. A captured subtree that refers to a lambda parameter remains
scope- and owner-sensitive; it is not a detached tree that can safely be moved
outside its original lambda. Repeated holes compare bound references relative
to their corresponding ambient scopes and preserve free-symbol identity.

The unpublished exact internal backend lowers this same bounded Lambda1 shape
both source-free and with generated-origin positions. It resolves bound
references through project binder identity and consumes the completed
parameter-type sidecar. Nested lambdas and broader lambda/block syntax remain
outside that internal contract and fail closed.

## Ordinary-parameter definitions

Inside an active macro `Quotes`, the public reflected construction surface is
exactly:

```scala
val definition: q.reflect.DefDef =
  dqr"def id(x: $parameterType): $resultType = x"
```

Both holes are caller-owned `q.reflect.TypeRepr` values inspected through the
bounded neutral `TypeNormalForm`; their normalized forms must be equal. The
literal method and parameter names are validated ordinary identifiers, and the
literal body must name that parameter exactly. The same private binder-aware
single-parameter core validates this identity contract before public Quotes
lowering creates `MethodType`, a method symbol under `Symbol.spliceOwner`, its
owned parameter symbol, and a body reference to that exact parameter symbol.

The returned `DefDef` is caller-owned and supports only immediate placement in
a local `Block` produced by the same macro invocation. It is not detached or
portable across Quotes universes and provides no arbitrary owner/member/class/
package placement, subtree reownership, or owner repair. There are no name,
body, sequence, whole-definition, type-parameter, contextual-parameter,
multi-clause, multi-parameter, or exact-two construction holes.
All rejected templates owned by this surface abort with
`Invalid dqr definition template:`; there is no successful source-evidence
wrapper.

The public definition matcher may be used through the bounded extractor:

```scala
target match
  case dqq"def id(x: Int): String = $body" =>
    val originalBody: q.reflect.Term = body
  case _ => ()
```

or configured programmatically with exactly:

```scala
DefinitionPattern.singleParameter(
  "def id(x: Int): String = $body"
)
```

Its method and parameter names are fixed ordinary identifiers. Its parameter
and result types are fixed members of the existing bounded type grammar and
may differ. `$body` is the only hole and must occupy the complete RHS. Name,
type, sequence, repeated, fragment, whole-definition, type-parameter,
contextual, default, varargs, extra-parameter, and extra-clause forms are not
admitted.

`matchDefinition` and the `dqq` extractor accept a caller-owned
`q.reflect.DefDef` with one ordinary parameter. They check the exact names,
bounded type normal forms, RHS presence, and the method/parameter symbol-owner
relationship. A target difference or an unsupported target type returns
`None`; in a `dqq` match this falls through normally. On success, `dqq`
captures exactly `target.rhs.get`; the programmatic result additionally exposes
the exact original `parameter.tpt.tpe` and `target.returnTpt.tpe`. The captured
body is unconstrained and may contain bound or free references, so it remains
owner-sensitive and must not be treated as a detached tree. No symbols or
owners are exposed, and the matcher performs no construction, owner mutation,
or reparenting. This is not general definition matching: there are no name,
type, partial-body, sequence, whole-definition, multi-parameter, contextual,
default, varargs, type-parameter, or multi-clause captures.

The public compiler-free first-use surface admits the identity-like subset:

```scala
def id(x: Int): Int = x
def keep(x: String): String = x
```

Call `CompletedTerm.definitionParameterReference("x")` and pass the result to
`DefinitionConstruction.singleParameterMethod`. A plain
`CompletedTerm.reference("x")` remains a free stable reference and is rejected
by this definition constructor. Parameter and result types must be equal for
the parameter-reference body, and both must belong to the existing bounded
constructible type family.

The package-private compiler-free definition model additionally admits bodies
from the existing bounded internal term family, including:

```scala
def id(x: Int): Int = x
def inc(x: Int): Int = x + 1
def keep(x: String): String = x
```

The method name, one ordinary parameter name, structural parameter type,
structural result type, and existing definition-body subset are explicit. The
parameter declaration and its body references share the existing project-owned
binder identity. Therefore renaming `x` to `y` preserves structural equality
when corresponding references remain bound, while a free same-text identifier
remains distinct. Parameter/result types, the definition name, and non-bound
body structure remain significant.

The public result is a projection, not the internal definition value. It exposes
only the method kind, names, completed types, explicit body projection, and a
coherent source rendering. `BinderId`, `TermShape`, constructed/template
definitions, source maps, and compiler trees remain package-private.

This is not a general parameter-list model. Multiple clauses, more than two
ordinary parameters, contextual/implicit/type parameters, defaults, varargs,
by-name or erased parameters, dependent methods, local definitions,
binder-name holes, and general owner/placement policy remain unsupported.
Richer public bodies and a source adapter are not implied. The existing located
definition carrier cannot truthfully describe parameter and parameter-type
spans for these variants, so it rejects them.

The unpublished exact internal backend supports the single-parameter shape in
two modes. Source-free lowering constructs one ordinary parameter `ValDef` and
a `DefDef` directly with no source, meaningful span, symbol, owner, parser, or
typer claim. Generated-origin lowering renders deterministic ordinary Scala
and recursively assigns parser-equivalent positions under one virtual source.
Both modes resolve bound references by the project binder identity and emit the
validated parameter declaration spelling, while free same-text identifiers
remain free. Foreign binder identities and missing, unsupported, or unconsumed
completed type sidecars fail closed. The backend does not expand the admitted
parameter-list syntax or make a general placement/owner promise.

The public compiler-free surface also admits exactly two ordered ordinary
parameters when the body explicitly returns either declared parameter:

```scala
def first(x: Int, y: String): Int = x
def second(x: Int, y: String): String = y
```

Call `DefinitionConstruction.twoParameterMethod` with distinct parameter names
and a `CompletedTerm.definitionParameterReference` naming one of them. The
result type must equal the selected parameter type. The public name is resolved
once to the corresponding internal binder identity; unknown names and free
same-text references fail with `invalid-two-parameter-method-contract`.

The exact-two public result preserves first/second names and types in source
order. It remains a projection over the package-private compiler-free model.
Its unpublished exact internal backend constructs one ordered two-parameter
clause directly in source-free mode and a canonical, recursively positioned
tree in generated-origin mode. Binder identity, rather than display text,
selects either declaration; free same-text references remain free. This is an
exact-two adapter only: it adds no general-arity, alternate-clause, ownership,
placement, typing, or public compiler-tree contract.

Unsupported type inputs report the rejected constructor, selected syntax,
expected arity, or unsupported family where available. Located source adapters
use exact-occurrence precision only when the existing source map identifies a
single truthful occurrence; otherwise they use whole-source precision or no
location. Generated hole-transport identifiers are not part of public errors.

## Constructor expressions

Term construction and matching support one bounded form:

```scala
new java.lang.StringBuilder(16)
```

The class name must be fully qualified, plain, and non-generic, with exactly
one ordinary argument list. Arguments may use the already-supported term
subset. Imported/simple names, generic types, constructor holes, named or
multiple lists, and anonymous classes are rejected. This is constructor-only
resolution, not a general type-name resolver.
