# Syntax support matrix

This is a project-specific map of tested support. It is not a compatibility
claim with Scala 2 quasiquotes, Scalameta, or the complete Scala grammar.

Status vocabulary:

- `SUPPORTED` — available through the named public surface for this family;
- `BOUNDED` — available only under the stated structural restrictions;
- `INTERNAL` — represented or lowered internally but not exposed as public syntax;
- `NOT_YET` — outside the current implementation, without a permanent exclusion;
- `NOT_PLANNED` — deliberately absent from the current product direction.

## Terms / expressions

| Family | Example | Construction | Matching | Public surface | Status / limits |
| --- | --- | --- | --- | --- | --- |
| Literals | `1`, `"text"`, `true` | Yes | Yes | `qr`, `QuasiPattern.term` | `SUPPORTED`; bounded literal kinds, no constant folding |
| Identifiers | `value` | Yes | Yes | `qr`, `QuasiPattern.term` | `SUPPORTED`; structural names and explicit holes |
| Selection | `value.size` | Yes | Yes | `qr`, `QuasiPattern.term` | `SUPPORTED`; no overload or semantic member equivalence |
| Application | `f(x)` | Yes | Yes | `qr`, `QuasiPattern.term` | `SUPPORTED`; ordinary supported argument lists |
| Infix | `left + right` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; supported operators, structural rather than algebraic equality |
| Unary | `-x`, `!flag` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; `+`, `-`, `!`, and `~`, with parser folding boundaries |
| Ascription | `value: Int` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; supported type family and construction-only type splices |
| Tuples | `(a, b)` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; term arities 2–22, ordered structural equality |
| Conditional | `if c then a else b` | Yes | Yes | `qr`, `QuasiPattern.term` | `SUPPORTED`; no branch simplification or control-flow equivalence |
| Standard interpolation | `s"hello $name"` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; standard single-quoted `s` only, with layered-dollar holes |
| Constructor | `new java.lang.StringBuilder(16)` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; fully-qualified, non-generic name and one ordinary argument list |
| Lambda1 | `(x: Int) => x` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; exactly one explicitly typed ordinary parameter, alpha-aware |
| Binder-free P1 block | `{ effect1(); effect2(); result }` | Yes | Yes | `qr`, `qq`, `QuasiPattern.term` | `BOUNDED`; one or more ordered expression prefixes plus a distinct final result; children must already be admitted Terms |
| Single typed local immutable val (P2) | `{ val x: Int = init; use(x) }` | Yes | Yes | `qr`, `qq`, `QuasiPattern.term` | `BOUNDED`; exactly one eager immutable simple binder in the whole tree with an explicit admitted type; initializer is outside scope, only the final result sees the binder, and P2/Lambda1 same-name source shadowing is rejected |
| Ordered term capture extractor | `case qq"$left + $right"` | No | Yes | `qq`, with `QuasiPattern.term` retained | `BOUNDED`; at least one distinct term slot, captures in source order, mismatch falls through |
| Other local values / local definitions | `{ val x = 1; x }`, `{ var x: Int = 1; x }`, `{ def x = 1; x }` | No | No | — | `NOT_YET`; inferred, mutable, lazy, pattern, multiple/shadowing, recursive, and local-method forms remain excluded |
| Match / try / loops / for | `value match ...` | No | No | — | `NOT_YET`; no broad control-flow surface |
| General term AST | arbitrary Scala expression | No | No | — | `NOT_PLANNED`; this project intentionally exposes a bounded structural subset |

Detailed caveats and equality rules live in
[Supported syntax and limitations](SUPPORTED_SYNTAX_AND_LIMITATIONS.md).

The ordinary family rows describe the recoverable programmatic matcher
`QuasiPattern.term` (and `termLocated` / `termOrThrow`). The `qq` row is the
independent ergonomic extractor dimension: it reuses that matcher for a
template with at least one interpolated term slot, assigns slots distinct
ordinal identities, and returns caller-owned `quotes.reflect.Term` captures.
It does not add type slots, sequence splices, backreferences, definition
patterns, or general Scala quasiquote coverage.

## Types

`QuasiTypequotes.*` deliberately exports both the recoverable neutral function
forms and the Quotes-dependent interpolator/extractor overloads.

| Family | Example | Construction | Matching | Public surface | Status / limits |
| --- | --- | --- | --- | --- | --- |
| Named types | `Int`, `String`, `Boolean` | Yes | Yes | `tqr`, `tqq`, structural APIs | `SUPPORTED`; syntactic structural identity |
| Fixed applied types | `Either[List[Int], Option[String]]` | Yes | Yes | `tqr`, `tqq`, structural APIs | `BOUNDED`; only `List`/1, `Option`/1, and `Either`/2 |
| Tuple types | `(Int, String)`, `(Int, String, Boolean)` | Yes | Yes | `tqr`, `tqq`, structural APIs | `BOUNDED`; Tuple2 and Tuple3 |
| Function types | `Int => String`, `(Int, String) => Boolean` | Yes | Yes | `tqr`, `tqq`, structural APIs | `BOUNDED`; Function1 and Function2 |
| Type holes | `Either[$left, $right]` | Yes | Yes | programmatic `tqr`, `tqq` | `BOUNDED`; named whole-type positions and repeated-hole structural equality |
| Ordered reflected type construction | `tqr"Either[$left, $right]"` | Yes | No | interpolated `tqr` | `BOUNDED`; zero or more distinct ordinal `TypeRepr` slots, fixed constructors only |
| Ordered reflected type capture extractor | `case tqq"Either[$left, $right]"` | No | Yes | interpolated `tqq` | `BOUNDED`; zero or more distinct ordinal slots, original target subtrees, mismatch falls through |
| Canonical global selected terminals | `some.pkg.TopLevel`, `some.pkg.Owner.Nested` | Yes | Yes | explicit `GlobalSelectedTypeEnvironment` + `GlobalSelectedTypeFrontend` | `EXPERIMENTAL_BOUNDED`; typed-witness-derived canonical Package/Type/Module ownership only |
| Canonical selected fixed constructors | `scala.collection.immutable.List[Int]`, `scala.Option[String]`, `scala.util.Either[Int, String]` | Yes | Yes | explicit environment-aware programmatic surface | `EXPERIMENTAL_BOUNDED`; exact declaration identity and existing arities/child forms only |
| Stable-term path-dependent types, aliases, alternate spellings | `value.Type`, alias source paths, import-shortened paths | No | No | — | `NOT_YET`; requires prefix identity or sound spelling validation; ordinary `tqr`/`tqq` still reject selected syntax |
| Wildcards, refinements, match types | `List[?]`, `A { ... }`, `T match ...` | No | No | — | `NOT_YET`; outside the bounded normal form |

The interpolated forms reuse the same normal-form construction and matching
semantics. Their Scala splice/capture binder spelling is not semantic identity:
slots are assigned distinct left-to-right ordinals. The programmatic pattern
`QuasiTypequotes.tqq("Either[$same, $same]")` retains repeated named-hole
equality. Unsupported reflected targets return `None` from the extractor; an
unsupported splice or malformed/unsupported template fails with a controlled
compile-time diagnostic.

## Definitions

Public definitions include ordinary `DefinitionConstruction` functions and
read-only projections plus one exact caller-owned reflected interpolator.

| Family | Example | Core representation | Public construction | Exact backend | Status / limits |
| --- | --- | --- | --- | --- | --- |
| Contextual method projection | `def apply[A](using x: Show[A]): Show[A] = x` | Bounded public value | Yes | Internal bounded adapter | `BOUNDED`; one type parameter and one contextual parameter |
| Parameterless definition | `def answer: Int = 42` | Yes | No | Internal bounded adapter | `INTERNAL`; no public projection |
| Immutable value | `val answer: Int = 42` | Yes | No | Internal bounded adapter | `INTERNAL`; no public projection |
| Single ordinary parameter | `def id(x: Int): Int = x` | Yes | Yes | Internal exact source-free and generated-origin modes | `BOUNDED`; public body selects the declared parameter |
| Exactly two ordinary parameters | `def first(x: Int, y: String): Int = x` | Yes | Yes | Internal exact source-free and generated-origin modes | `BOUNDED`; one ordered list, distinct names, binder-aware bounded body |
| Public reflected identity definition interpolation | `dqr"def id(x: $parameterType): $resultType = x"` | Binder-aware bounded core validation | Yes, caller-owned `DefDef` | Public Quotes lowering only | `BOUNDED`; one clause/parameter, two equal supported `TypeRepr` slots, literal parameter body, immediate same-Quotes local placement |
| Programmatic single-parameter definition matching | `DefinitionPattern.singleParameter("def id(x: Int): Int = $body")` | Private neutral comparison keys only | Yes | Public Quotes inspection | `BOUNDED`; fixed ordinary names/types, complete RHS capture, mismatch is `None`, original reflected values preserved |
| Definition pattern interpolation | `case dqq"def id(x: Int): Int = $body"` | Private neutral comparison keys only | Yes, caller-owned RHS `Term` | Public Quotes inspection | `BOUNDED`; one fixed ordinary method and parameter, fixed bounded types, one complete RHS capture, ordinary mismatch falls through |
| Curried, contextual exact-two, defaults, varargs, general arity | broader methods | No | No | No | `NOT_YET`; no general method-definition claim |

## Comparison references

- [Scala 2 quasiquote syntax summary](https://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html)
- [Scalameta quasiquotes](https://scalameta.org/docs/trees/quasiquotes)

These references motivate discoverability and table structure only. This
matrix describes this repository's own tested boundaries.

## Contributor rule

Any change that adds, removes, or materially alters a term, type, or definition
syntax family must update this matrix and the detailed limitations document in
the same change.
