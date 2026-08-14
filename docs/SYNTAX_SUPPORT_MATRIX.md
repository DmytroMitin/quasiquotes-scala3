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
| Blocks and local definitions | `{ val x = 1; x }` | No | No | — | `NOT_YET`; requires ownership and local-scope policy |
| Match / try / loops / for | `value match ...` | No | No | — | `NOT_YET`; no broad control-flow surface |
| General term AST | arbitrary Scala expression | No | No | — | `NOT_PLANNED`; this project intentionally exposes a bounded structural subset |

Detailed caveats and equality rules live in
[Supported syntax and limitations](SUPPORTED_SYNTAX_AND_LIMITATIONS.md).

## Types

`QuasiTypequotes.tqr(...)` and `QuasiTypequotes.tqq(...)` are functions, not
interpolators.

| Family | Example | Construction | Matching | Public surface | Status / limits |
| --- | --- | --- | --- | --- | --- |
| Named types | `Int`, `String`, `Boolean` | Yes | Yes | `tqr`, `tqq`, structural APIs | `SUPPORTED`; syntactic structural identity |
| Fixed applied types | `Either[List[Int], Option[String]]` | Yes | Yes | `tqr`, `tqq`, structural APIs | `BOUNDED`; only `List`/1, `Option`/1, and `Either`/2 |
| Tuple types | `(Int, String)`, `(Int, String, Boolean)` | Yes | Yes | `tqr`, `tqq`, structural APIs | `BOUNDED`; Tuple2 and Tuple3 |
| Function types | `Int => String`, `(Int, String) => Boolean` | Yes | Yes | `tqr`, `tqq`, structural APIs | `BOUNDED`; Function1 and Function2 |
| Type holes | `Either[$left, $right]` | Yes | Yes | `tqr`, `tqq` | `BOUNDED`; whole admitted type positions and repeated-hole structural equality |
| Selected/path-dependent types | `pkg.Type`, `value.Type` | No | No | — | `NOT_YET`; requires an explicit resolver and prefix policy |
| Wildcards, refinements, match types | `List[?]`, `A { ... }`, `T match ...` | No | No | — | `NOT_YET`; outside the bounded normal form |

## Definitions

Public definitions are ordinary `DefinitionConstruction` functions and
read-only projections. There is no public definition string interpolator.

| Family | Example | Core representation | Public construction | Exact backend | Status / limits |
| --- | --- | --- | --- | --- | --- |
| Contextual method projection | `def apply[A](using x: Show[A]): Show[A] = x` | Bounded public value | Yes | Internal bounded adapter | `BOUNDED`; one type parameter and one contextual parameter |
| Parameterless definition | `def answer: Int = 42` | Yes | No | Internal bounded adapter | `INTERNAL`; no public projection |
| Immutable value | `val answer: Int = 42` | Yes | No | Internal bounded adapter | `INTERNAL`; no public projection |
| Single ordinary parameter | `def id(x: Int): Int = x` | Yes | Yes | Internal exact source-free and generated-origin modes | `BOUNDED`; public body selects the declared parameter |
| Exactly two ordinary parameters | `def first(x: Int, y: String): Int = x` | Yes | Yes | Internal exact source-free and generated-origin modes | `BOUNDED`; one ordered list, distinct names, binder-aware bounded body |
| Public definition source interpolation | `dqr"..."` | Internal research only | No | — | `INTERNAL`; package-private and not public API |
| Definition pattern interpolation | `dqq"..."` | No | No | No | `NOT_YET`; no such surface exists |
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
