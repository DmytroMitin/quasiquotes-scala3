# Typed sequence Term splice design

This document records a compile-checked design decision. Sequence Term
splicing is **not** a current public feature: the production `qr` surface still
accepts only one reflected Term per ordinary Term interpolation slot.

## Selected future source and host contract

The preferred source spelling is the explicit Scala-2-style rank marker:

```text
qr"f(..$args)"
qr"new $constructorType(..$args)"
```

The host argument should be a dedicated public sequence-Term carrier whose
type parameter is the caller's active `q.reflect.Term` path. A conceptual
minimal shape is:

```text
TermSequenceSplice[+Term]
TermSequenceSplices.termSplice(terms: Seq[Term])
```

This is an API sketch, not a shipped declaration. The source rank marker and
the carrier are both required. The marker makes repeated source topology
visible; the carrier prevents every `Seq` from becoming an interpolation
argument and gives wrong-position diagnostics a distinct category.

Adding `Seq[q.reflect.Term]` directly to the `qr` argument union is rejected as
the selected design because it admits sequence rank accidentally. A general
rank ADT is also deferred: it would imply policy for sequence Types,
Definitions, statements, or argument lists before those categories have been
designed.

## Host-language evidence

Test-only macros compile and run on Scala 3.3.8, 3.8.4, and 3.9.0-RC1. On all
three lines:

- `f(..$args)` reaches a custom interpolator as `StringContext("f(..", ")")`
  plus exactly one host argument;
- `new C(..$args)` analogously preserves `..` in the literal part;
- `f($wrapped)` reaches the same one-argument host shape but hides sequence
  rank from the quasiquote source;
- `${args*}` is not a legal custom-interpolator argument expression.

The Scala 3 guest parser does not parse `f(..placeholder)`. The future
interpolator must therefore classify a sequence carrier, require the adjacent
`..` marker, and consume that marker while generating one ordinary identifier
placeholder **before** the existing single parse. It must not parse once to
discover rank and again after rewriting.

## First construction gate

The selected first gate covers both:

- an ordinary `Apply` argument list; and
- the existing one-list `New` form, including the already supported complete
  constructor `TypeRepr` position.

The two positions share one ordered list-expansion primitive. Constructor
selection remains the current exact-compiler policy. Multiple, named, or
contextual argument lists and target-method vararg expansion remain outside
this gate.

The bounded topology is:

- empty, one-element, and multi-element sequences are admitted;
- fixed ordinary terms before and after one repeated hole are admitted;
- exactly one repeated Term hole is admitted per argument list initially;
- a repeated Term carrier without its adjacent marker, a marker attached to a
  non-sequence carrier, or a repeated hole outside an Apply/New argument list
  fails deterministically;
- two repeated holes in one list are initially rejected rather than assigned
  implicit concatenation semantics.

## Identity and ownership

Test-only public-reflection probes on all three compiler lines construct empty,
one-element, multi-element, and fixed-around Apply and New argument lists.
They preserve order and retain the exact input Term objects for fixed-arity
targets. The sequence includes a literal, a caller-local reference, a Term
returned by an earlier `qr`, and a block expression containing a source-owned
local definition.

The design therefore applies the existing single-Term rule element by element:
Terms remain in the same caller `Quotes` universe, no print/reparse or neutral
`TermShape` projection occurs, and no generic `changeOwner` is introduced. A
whole valid expression subtree containing owned definitions is not itself a
definition statement to re-own. Existing position-specific fail-closed owner
rules remain unchanged.

## Parser and template boundary

Completed semantic `TermShape.Apply.arguments` and `TermShape.New.arguments`
must remain ordinary ordered Term vectors. A sequence hole is not a Term and
must never survive in a completed semantic tree.

For the direct reflected `qr` path, the smallest implementation is a distinct
placeholder category plus argument-list-aware expansion in the frontend:

```text
interpolation classification
  -> ranked sequence placeholder in one Apply/New argument list
  -> one guest parse with the `..` marker already consumed
  -> ordered reflected-Term expansion
  -> ordinary reflected Apply/New
```

The compiler-free `TermTemplate` core remains unchanged in the first gate. If
compiler-free sequence construction is later selected, its argument template
needs an explicit list-element model such as fixed subtree versus repeated
hole. Adding a non-tree splice node to final `TermShape` is not acceptable.

## Matching and other ranks

Sequence capture in `qq` is a separate next gate. Target inspection already
preserves ordered Apply/New children, but the current `unapplySeq` surface
returns one `q.reflect.Term` per source capture slot. A repeated capture needs
an explicit capture-result contract and pattern-list matching rules, including
empty capture and fixed prefix/suffix behavior. It should not be smuggled into
construction through an untyped union.

This design does not deliver or imply:

- sequence Type splices;
- sequence Definition, member, or statement splices;
- rank-2 argument-list splices;
- raw `untpd` holes; or
- typed Scalameta parity.

A future hybrid Scalameta implementation may reuse the rank/category and list
expansion semantics, but caller-owned reflected Terms must not pass through
the neutral `ScalametaTermProjection`.
