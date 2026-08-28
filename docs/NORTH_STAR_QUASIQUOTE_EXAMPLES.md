# North-star quasiquote examples

These checkpoints preserve high-value cases where public Scala 3 reflection is
substantially more mechanical than the source shape a macro author wants. They
are design targets, not current syntax or implementation commitments.

All conceptual quasiquote syntax in this document is future, non-current
notation. It illustrates semantic roles and does not select the final syntax.

The current public boundary remains the [syntax support matrix](SYNTAX_SUPPORT_MATRIX.md).
Conceptual `$T`, `..$arguments`, `..$types`, and `..$definitions` spellings
below name semantic roles only. A later design gate must select any exact
interpolator grammar, ownership model, and error contract.

## Checkpoint status

| Checkpoint | Manual baseline | Current quasiquote coverage | Remaining status |
| --- | --- | --- | --- |
| N1 generic subclass with override | documented public-reflection machinery; compact cross-line fixture deferred | narrow method-definition pieces only | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |
| N2 dynamic Type application | `CURRENT_MANUAL_BASELINE_PROVED` | fixed constructors and fixed-arity whole-Type holes only | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |
| N3 generated Type refinement members | `CURRENT_MANUAL_BASELINE_PROVED` | parser/shape evidence only; no public refinement construction | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |
| N4 anonymous implementation body | documented reflection/definition problem | bounded individual method surfaces only | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |
| N5 dynamic existing-type construction | `CURRENT_MANUAL_BASELINE_PROVED` | fixed constructor plus caller-owned complete constructor `TypeRepr` with one ordinary argument list | `COMPLETE_CONSTRUCTOR_TYPE_SPLICE_IMPLEMENTED`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |

`PARTIALLY_COVERED_BY_CURRENT_QUASIQUOTES` means only that a smaller structural
subproblem exists today. None of N1-N5 is `CHECKPOINT_COMPLETE`.

## N1 — generic subclass with override

### Manual/current baseline

A macro has an abstract `Type[A]` and must synthesize a fresh subclass of `A`
with an override. Ordinary source quotation cannot write `new A: ...` merely
because an abstract `A: Type` exists. Public reflection planning includes
`Symbol.newClass`, `Symbol.newMethod`, `ClassDef`, `DefDef`,
`Apply(Select(New(...), primaryConstructor), ...)`, a containing `Block`, and
correct class/member/parameter owner and binder wiring.

This is the most compiler-sensitive baseline in the portfolio. It is retained
as reviewed API-level machinery rather than forced into a disproportionately
large cross-version fixture in this documentation phase.

### Desired source-like shape

A future class/definition/term quotation should express the owned class,
parent, constructor invocation, and override in source order. The typed backend
should create derivable symbols and owners rather than making the caller repeat
routine `Symbol.newClass` and `Symbol.newMethod` wiring.

### Required missing capabilities

- class and anonymous-class definition support;
- method override definitions;
- a dynamic parent Type position;
- constructor/new lowering;
- class body and broader Definition support;
- staged backend symbol and owner planning.

This is class synthesis, not the existing-type constructor application in N5.

### Checkpoint criterion

The checkpoint completes only when supported source-like code constructs the
owned subclass and override on every promised compiler line, with owner,
binder, typechecking, and controlled-failure tests replacing the manual symbol
plan. Merely rendering plausible syntax is insufficient.

## N2 — dynamic type application with runtime-length Type arguments

### Manual/current baseline

A macro computes a type constructor and a runtime-length `List[TypeRepr]`, for
example field types recovered from `Mirror.ProductOf[A]` and
`MirroredElemTypes`. Public reflection constructs the result directly:

```scala
AppliedType(constructor, arguments)
```

The external-package manual-baseline fixture constructs a binary example from
a list whose length is observed at macro expansion. Standard type quotations
otherwise require repeated `asType` and kind-aware existential plumbing.

### Desired source-like shape

```text
tqr"$constructor[..$arguments]"
```

This is conceptual notation. It is stronger than the current fixed-arity
`tqr"Either[List[$left], Option[$right]]"` example.

### Required missing capabilities

- a Type constructor-position splice/hole;
- a sequence or variadic Type splice;
- constructor kind and arity validation;
- exact capture/ownership and controlled error semantics;
- parity on overlapping current-Dotty and Scalameta claimed slices.

### Checkpoint criterion

The checkpoint completes when a runtime-length Type argument list can replace
manual `AppliedType` through supported source-like syntax on the promised
compiler lines, with valid and wrong-kind/arity cases and truthful cross-route
parity evidence.

## N3 — generated Type refinement members

### Manual/current baseline

The target is a Type equivalent to:

```scala
A { type Out1 = B1; type Out2 = B2 }
```

Public reflection represents each member through nested `Refinement` values.
An alias uses `TypeBounds` whose lower and upper bounds are the same aliased
Type. The external-package fixture proves one independently constructed alias
is semantically equal to `AnyRef { type Out = String }`; the runtime-length
member list remains the source-like checkpoint.

### Desired source-like shape

```text
tqr"$parent { ..$typeMembers }"
```

This conceptual Type quotation must remain separate from anonymous-class body
generation: a refinement returns a Type/`TypeRepr`, not a Term or class body.

### Required missing capabilities

- an admitted refinement Type family;
- a type-member definition representation;
- a sequence splice for type definitions;
- dynamic names when member names are computed;
- explicit alias-versus-bounds `TypeBounds` semantics;
- ownership and binder rules where the chosen representation requires them.

### Checkpoint criterion

The checkpoint completes when supported Type quasiquotes construct and inspect
computed refinement members on every promised compiler line, distinguish
aliases from abstract bounds, preserve dynamic-name/error rules, and replace
the manual nested `Refinement` baseline.

## N4 — anonymous implementation with a calculated definition body

### Manual/current baseline

A type-class materializer or derivation may need an anonymous implementation
whose body contains a runtime-length collection of generated type aliases
and/or methods. Reflection must assemble the anonymous class, constructor, and
owned definitions, including any symbols required by a typed backend.

### Desired source-like shape

```text
qr"new FieldTypes[A]: ..$definitions"
```

`FieldTypes` and `Out` are motivations, not new current APIs. The present
`dqr` surface does not accept arbitrary type definitions, whole definitions,
or sequence splices.

### Required missing capabilities

- anonymous-class/new body syntax;
- broader Definition ADTs;
- whole-definition or sequence Definition splices;
- dynamic Type/member names when computed;
- typed-backend symbol and owner planning for owned definitions;
- hygiene, capture, and placement rules.

### Checkpoint criterion

The checkpoint completes when a supported source-like anonymous implementation
accepts a calculated definition sequence and passes exact owner, hygiene,
typing, placement, and controlled-negative tests on every promised line.

## N5 — low-level product derivation with dynamic `new T(..args)`

### Manual/current baseline

A product decoder or deriver has an existing computed product Type and a
runtime-length collection of constructor arguments. The reflection-heavy
subproblem is assembly through `New`, constructor `Select`, `Apply`, possible
`TypeApply`, literals, and type-directed conversions or casts. The
external-package fixture is generic in `Type[T]`, turns that computed
`TypeRepr` back into a type with `asType`, and independently constructs an
existing product from a macro-built `List[Term]`; it does not synthesize a
class or call `Symbol.newClass`.

### Desired source-like shape

```text
qr"new $T(..$arguments)"
```

The notation is conceptual and does not select constructor-resolution or
coercion policy.

### Required missing capabilities

- a dynamic Type splice in constructor/type position;
- a sequence or variadic Term argument splice;
- constructor resolution and selection rules;
- argument typing and coercion policy;
- fail-closed behavior for unsupported or ambiguous constructors.

### Checkpoint criterion

The checkpoint completes when supported source-like construction replaces the
manual existing-type `New`/`Select`/`Apply` sequence for runtime-length
arguments on every promised compiler line, with constructor, arity, argument
type, ownership, and failure tests.

## Copyright and provenance

The broader Decoder motivation cites [Stack Overflow answer 62861463](https://stackoverflow.com/a/62861463).
This repository does not copy or silently adapt that full third-party example.
The compile-checked N5 fixture is independently authored and isolates only the
public-reflection `new T(..args)` problem. Any future substantial adaptation
requires an explicit attribution and license review before inclusion in this
Apache-2.0 repository.

## Portfolio priority

The explicit-receiver dynamic selected-member construction gap is implemented
through a validated decoded-name value and unique selection lowering. It does
not add bare-name lookup, overload resolution, dynamic infix syntax, or name
matching.

The complete constructor Type position in N5 is implemented. Its input is a
caller-owned `TypeRepr`, including a direct `tqr` result, with no `Any` or
public wrapper carrier. This slice retains one ordinary constructor argument
list and the existing exact-compiler constructor policy; N5 remains incomplete
because sequence arguments and the broader dynamic applied-Type constructor
family are absent. Refinements, classes, and definitions remain separate
gates. Source-owned local `def` statements are the next product-facing
Definition-composition candidate, while external typed-definition splicing
requires a separate explicit reownership contract.
