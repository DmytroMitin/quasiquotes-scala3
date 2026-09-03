# Syntax support matrix

This is a project-specific map of tested support. It is not a compatibility
claim with Scala 2 quasiquotes, Scalameta, or the complete Scala grammar.
It is the user-facing Q syntax view. The independent typed-Scalameta, neutral,
fresh exact-lowering, and existing-tree rewrite directions are documented in
the [cross-surface capability matrix](CROSS_SURFACE_CAPABILITY_MATRIX.md).

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
| Selection | `value.size` | Yes | Yes | `qr`, `QuasiPattern.term` | `SUPPORTED`; fixed selection without an explicit argument list retains value-position normalization; no overload or semantic member equivalence |
| Dynamic selected-member name | `qr"$receiver.$selectedName()"`, `qr"$receiver.$selectedName($argument)"` | Yes | No | `SelectedMemberName.from`, `qr` | `BOUNDED`; validated decoded ASCII name, explicit receiver selection only, unique accessible member only; explicit empty and nonempty calls are supported; no dynamic infix, lexical lookup, overload resolution, or name capture |
| Application | `f(x)`, `builder.capacity()` | Yes | Yes | `qr`, `QuasiPattern.term` | `SUPPORTED`; ordinary supported argument lists, including an explicit empty application over a selected nullary method |
| Sequence-Term arguments | `qr"f(fixed, ..$args)"`, `qr"new $typeRepr(..$args)"`, `case qq"f($head, ..$tail)"`, `case qq"new fixed.Type(..$args)"` | Yes | Yes | `TermSequenceSplices.termSplice`, `qr`, `qq` | `BOUNDED`; construction uses one dedicated caller-universe carrier per ordinary Apply or supported one-list New argument list; matching admits exactly one direct ordinary Apply-argument or fixed one-list New-argument capture as `Seq[q.reflect.Term]`; empty/one/many and fixed prefix/suffix supported; no dynamic/type-applied/multi-clause ranked New, tuple/interpolation/block/Type/Definition matching, rank 3, or vararg-star semantics |
| Infix | `left + right` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; supported operators, structural rather than algebraic equality |
| Unary | `-x`, `!flag` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; `+`, `-`, `!`, and `~`, with parser folding boundaries |
| Ascription | `value: Int` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; supported type family and construction-only type splices |
| Tuples | `(a, b)` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; term arities 2–22, ordered structural equality |
| Conditional | `if c then a else b` | Yes | Yes | `qr`, `QuasiPattern.term` | `SUPPORTED`; no branch simplification or control-flow equivalence |
| Standard interpolation | `s"hello $name"` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; standard single-quoted `s` only, with layered-dollar holes |
| Constructor | `new java.lang.StringBuilder(16)`, `new $typeRepr(16)` | Yes | Yes for fixed source Type only | `qr`, `QuasiPattern.term` | `BOUNDED`; fixed fully-qualified non-generic name or caller-owned `TypeRepr` as the complete constructor Type, one ordinary argument list; reflected-Type matching is absent |
| Lambda1 | `(x: Int) => x` | Yes | Yes | `qr`, `QuasiPattern.term` | `BOUNDED`; exactly one explicitly typed ordinary parameter, alpha-aware |
| Binder-free P1 block | `{ effect1(); effect2(); result }` | Yes | Yes | `qr`, `qq`, `QuasiPattern.term` | `BOUNDED`; one or more ordered expression prefixes plus a distinct final result; children must already be admitted Terms |
| Single typed local immutable val (P2) | `{ val x: Int = init; use(x) }` | Yes | Yes | `qr`, `qq`, `QuasiPattern.term` | `BOUNDED`; exactly one eager immutable simple binder in the whole tree with an explicit admitted type; initializer is outside scope, only the final result sees the binder, and P2/Lambda1 same-name source shadowing is rejected |
| Source-owned local identity method | `{ def id(x: $parameterType): $resultType = x; id(arg) }` | Yes | No | `qr` | `BOUNDED`; construction only, exactly one literal method with one ordinary parameter, complete caller-owned `TypeRepr` parameter/result positions, parameter-reference body, and one following result; binder identity creates the following method reference |
| Ordered term capture extractor | `case qq"$left + $right"`, `case qq"$function(..$arguments)"`, `case qq"new fixed.Type(..$arguments)"` | No | Yes | `qq`, with `QuasiPattern.term` retained | `BOUNDED`; scalar slots bind `q.reflect.Term`; exactly one admitted direct Apply-argument or fixed one-list New-argument rank-2 slot binds `Seq[q.reflect.Term]`; captures remain in source order and mismatch falls through |
| Other local values / local definitions | `{ val x = 1; x }`, `{ var x: Int = 1; x }`, recursive/multi-clause/multiple local methods | No | No | — | `NOT_YET`; inferred, mutable, lazy, pattern, multiple/shadowing, recursive, contextual, generic, multi-clause, and arbitrary-body local forms remain excluded |
| Match / try / loops / for | `value match ...` | No | No | — | `NOT_YET`; no broad control-flow surface |
| General term AST | arbitrary Scala expression | No | No | — | `NOT_PLANNED`; this project intentionally exposes a bounded structural subset |

Detailed caveats and equality rules live in
[Supported syntax and limitations](SUPPORTED_SYNTAX_AND_LIMITATIONS.md).

The ordinary family rows describe the recoverable programmatic matcher
`QuasiPattern.term` (and `termLocated` / `termOrThrow`). The `qq` row is the
independent ergonomic extractor dimension: it reuses that matcher for a
template with at least one interpolated term slot, assigns slots distinct
ordinal identities, and returns caller-owned `quotes.reflect.Term` captures.
It also admits exactly one rank-2 Term sequence capture directly in ordinary
`Apply.arguments` or the arguments of an existing fixed one-list `New`,
returning `Seq[quotes.reflect.Term]` in the corresponding capture position. It
does not add type slots, multiple/generalized ranks, backreferences, definition
patterns, dynamic/type-applied/multi-clause constructors, ranked tuple/
interpolation/block matching, or general Scala quasiquote coverage.

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
| Ordered reflected type construction | `tqr"Either[$left, $right]"` | Yes | No | interpolated `tqr` | `BOUNDED`; zero or more distinct ordinal `TypeRepr` slots, fixed constructors, plus zero-hole canonical globally selected class terminals such as `java.lang.StringBuilder` |
| Ordered reflected type capture extractor | `case tqq"Either[$left, $right]"` | No | Yes | interpolated `tqq` | `BOUNDED`; zero or more distinct ordinal slots, original target subtrees, mismatch falls through |
| Canonical global selected terminals | `some.pkg.TopLevel`, `some.pkg.Owner.Nested` | Yes | Yes | explicit `GlobalSelectedTypeEnvironment` + `GlobalSelectedTypeFrontend` | `EXPERIMENTAL_BOUNDED`; typed-witness-derived canonical Package/Type/Module ownership only |
| Canonical selected fixed constructors | `scala.collection.immutable.List[Int]`, `scala.Option[String]`, `scala.util.Either[Int, String]` | Yes | Yes | explicit environment-aware programmatic surface | `EXPERIMENTAL_BOUNDED`; exact declaration identity and existing arities/child forms only |
| Stable-term path-dependent types, aliases, alternate spellings | `value.Type`, alias source paths, import-shortened paths | No | No | — | `NOT_YET`; requires prefix identity or sound spelling validation; the zero-hole canonical class-terminal `tqr` case does not admit these forms, and ordinary `tqq` remains unchanged |
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
| Public reflected definition interpolation | `dqr"def id(x: $parameterType): $resultType = x"`; `dqr"def choose(x: $leftType, y: $rightType): $resultType = y"` | Binder-aware bounded core validation | Yes, caller-owned `DefDef` | Public Quotes lowering only | `BOUNDED`; unchanged variadic signature; exact-one keeps two equal supported `TypeRepr` slots; current-Dotty exact-two has three standalone Int/String/Boolean slots and a literal body selecting either ordered binder; immediate same-Quotes local placement; no general N-parameter parity |
| Programmatic single-parameter definition matching | `DefinitionPattern.singleParameter("def id(x: Int): Int = $body")` | Private neutral comparison keys only | Yes | Public Quotes inspection | `BOUNDED`; fixed ordinary names/types, complete RHS capture, mismatch is `None`, original reflected values preserved |
| Definition pattern interpolation | `case dqq"def id(x: Int): Int = $body"`; `case dqq"def choose(x: Int, y: String): String = $body"` | Private neutral comparison keys only | Yes, caller-owned RHS `Term` | Public Quotes inspection | `BOUNDED`; one same-spelling transparent-inline selector; static exact one -> `SingleParameterDefinitionPattern`; static structural exact two -> scalable `DefinitionPatternExtractor`; dynamic/non-static -> exact-one fallback; no `dqq2`/`dqq3`/`dqq4` or arity-numbered carrier; typed-Scalameta preserves the same bounded selector split |
| Curried/contextual methods, defaults, varargs, multiple clauses, general arity | broader methods | No | No | No | `NOT_YET`; no general method-definition claim |

## Comparison references

- [Scala 2 quasiquote syntax summary](https://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html)
- [Scalameta quasiquotes](https://scalameta.org/docs/trees/quasiquotes)

These references motivate discoverability and table structure only. This
matrix describes this repository's own tested boundaries.

## Contributor rule

Any change that adds, removes, or materially alters a term, type, or definition
syntax family must update this matrix and the detailed limitations document in
the same change.
