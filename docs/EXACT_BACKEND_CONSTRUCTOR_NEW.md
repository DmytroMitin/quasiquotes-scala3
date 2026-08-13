# Exact constructor/new backend

The unpublished exact backend supports the same bounded constructor family as
the public frontend. The only admitted constructor identity is a
fully-qualified plain non-generic name with one ordinary argument list, for
example:

```scala
new java.lang.StringBuilder(16)
```

This document describes raw untyped construction and generated-origin source
mapping. It does not add syntax, publish `dottyInternal`, expose compiler trees,
or establish a public placement API.

## Raw untyped shape

Across the tested exact compiler lines, the admitted source parses as:

```text
Apply[0..source-end, point=0]
  Select(<init>)[type-start..type-end, point=type-start]
    New[type-start..type-end, point=type-start]
      Select(... Select(Ident(first), segment) ..., class)
  argument-1
  ...
```

For `new java.lang.StringBuilder(16)`, the type path is
`Ident(java)[4..8]`, `Select(lang)[4..13, point=9]`, then
`Select(StringBuilder)[4..27, point=14]`. `New` and the `<init>` selection
both cover `4..27` with point 4; the outer application covers `0..31` with
point 0. Empty argument lists have no argument child. Parser-oracle nodes are
untyped and retain `NoSymbol`.

Nested applications, conditionals, nested constructors, and a syntactically
admitted unresolved path preserve the same raw structure. A raw shape is not
evidence that a class exists, is accessible, has a matching overload, or can
be placed at an arbitrary compiler location.

## Source-free construction contract

`ConstructedTermUntypedBackend` validates the stored name with the existing
constructor-name policy, folds its segments into direct `Ident`/`Select` type
syntax, builds `New`, selects `<init>`, and wraps it in `Apply`. Arguments use
the existing recursive lowering state, including typed-sidecar preorder.

Production does not parse source, resolve a class, invoke reflection, run a
typer, assign an owner, or insert a tree. Every returned material node and
descendant has `NoSource`, no meaningful span, `NoSymbol`, and no
`TypedSplice`.

## Generated source and positions

The generated-origin planner emits exactly:

```text
new <fully-qualified-name>(<canonical-argument-1>, ...)
```

Distinct `New`, constructor-select, and type-select plans preserve the raw node
identities and point conventions. The positioner maps the direct raw tree
recursively under one virtual `SourceFile`. Independent tests parse the
generated spelling and compare relevant node kinds, child order, name/arity
detail, spans, and source slices. Production never reparses generated text.

Definition raw and generated-origin backends inherit constructor support
through the existing term-body seam. This adds no definition syntax, owner, or
placement policy.

## Unsupported and malformed forms

Simple/imported names, generic or path-dependent constructors, constructor
holes, backticked or binary segments, multiple/named/using argument lists, and
anonymous templates remain unsupported. Broader block and nested-lambda
production syntax is outside this backend contract.

Hostile or manually malformed internal values return controlled errors for
null, simple, or malformed names; null argument lists/elements; unsupported
nested nodes; and missing or unconsumed type sidecars. There is no permissive
parser, resolver, typer, symbol, owner, or placement fallback.

## Artifact and compatibility boundary

The exact backend lives in `dottyInternal`, whose artifact is deliberately
unpublished. `core` remains compiler-free and `frontend` remains coupled to an
exact compiler line. Backend tests across several compiler versions are
revision-specific evidence, not a compatibility promise or remote release
claim. The current public API inventory is maintained separately in
[Public API shape compatibility review](API_COMPATIBILITY_REVIEW.md).
