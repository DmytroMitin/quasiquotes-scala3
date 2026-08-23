# Hybrid Scalameta term frontend experiment

`hybridScalametaFrontend` is an unpublished, compiler-coupled module that
exercises a bounded alternate syntax frontend for typed term construction and
matching. It is intentionally side by side with the current public `qr`/`qq`
implementation; public calls still use the existing Dotty engine.

## Architecture

The construction path synthesizes collision-safe term/type placeholders, parses
the resulting source through Scalameta 4.17.3 public parser and `scala.meta.Term`
APIs, validates the same source against the active exact compiler grammar, and
lowers the Scalameta AST directly through the active caller `Quotes`. It never
pretty-prints a Scalameta tree for reparsing. A term hole is replaced by the
exact supplied `quotes.reflect.Term`; supported type holes use the existing
project-owned constructed-type lowering.

The pattern path uses the existing collision-safe pattern-source protocol,
converts the parsed Scalameta term directly to the existing project-owned
`TermPattern`, and delegates matching to the existing `TermMatcher`. Successful
captures are therefore the exact original reflected subtrees of the target,
including generated targets without a usable source span.

## Bounded syntax and fallback

The alternate construction slice covers identifiers; integer, string, and
boolean literals; selection and application; unary infix application; tuples;
`if`; supported type ascription; standard `s` interpolation; ordinary term
holes; binder-free P1 blocks; and the existing constructed-type splice. A P1
block is one or more ordered expression prefixes plus its final result; local
values, local definitions, imports, and unrelated statement/control-flow forms
remain excluded. The matching slice covers the
corresponding admitted literals, identifiers, selection/application, unary
infix, tuples, `if`, supported ascription, P1 blocks, and ordinary captures.

A Scalameta parse failure may dispatch to the unchanged current engine. An
exact-compiler grammar rejection, unsupported Scalameta AST shape, or typed
lowering failure does not trigger fallback, because doing so could change the
accepted language or hide a semantic failure. The current engine remains the
exact-compiler oracle and the explicit reference implementation.

The selected dialect follows the active supported compiler line: Scala 3.8.4
uses Scalameta `Scala38`; Scala 3.3.8 uses the ordinary `Scala3` dialect, which
is the compatible upstream policy available in Scalameta 4.17.3. Regardless of
dialect breadth, syntax rejected by the active exact compiler is rejected.

## Dependency and compatibility boundary

The module depends on the existing `frontend` and unpublished
`neutralScalameta` projects and uses staging only in tests. It is marked
`publish / skip := true`. Neither `core` nor the published `frontend` depends on
it, so the selected published POMs and the 618-row `core`/`frontend` public API
baseline remain immutable. The Phase-115 source candidate has a reviewed
four-row additive `core`/`frontend` delta for the shared block model; the
Scalameta-only implementation leaks no additional public row.

The mechanical Term parity inventory now contains 36 rows: 30 supported and 6
explicitly nonpublic. The experiment passes its focused construction,
matching, fallback, dialect, macro, staging, generated-target, and P1 block
checks on Scala 3.3.8 and 3.8.4. This is
evidence for continued bounded evaluation, not authorization to retire the
current engine or migrate the public default.
