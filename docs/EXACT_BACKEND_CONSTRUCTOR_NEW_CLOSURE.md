# Phase 75 — Bounded Exact Constructor/New Backend Closure

Phase 75 closes the unpublished exact backend for the constructor family
selected in Phase 71. It adds no constructor syntax and no public API. The
only admitted identity remains a fully-qualified plain non-generic name with
one ordinary argument list.

## Refreshed raw oracle

Scala 3.8.4, 3.9.0-RC1, 3.3.8, and
`3.8.5-RC1-bin-20260405-9478256-NIGHTLY` agree on:

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
point 0. Empty argument lists have no argument child. Every parser-oracle node
is `NoSymbol`. Nested applications, conditionals, nested constructors, and a
syntactically valid unresolved `synthetic.unresolved.Widget` path preserve the
same structure.

## Source-free exact construction

`ConstructedTermUntypedBackend` validates the stored name with the existing
`ConstructorNamePolicy`, folds its segments into direct `Ident`/`Select` type
syntax, builds `New`, selects `<init>`, and wraps it in `Apply`. Arguments use
the existing recursive lowering state, including typed-sidecar preorder.
Production code does not parse source, resolve a class, invoke reflection, run
a typer, assign an owner, or insert a tree.

Every returned node and descendant has `NoSource`, no span, `NoSymbol`, and no
`TypedSplice`. A nonexistent but syntactically admitted path therefore lowers
successfully as raw syntax. That is not evidence of class existence,
accessibility, constructor overload selection, type correctness, or valid
placement.

## Generated canonical source and positions

The generated-origin planner emits exactly:

```text
new <fully-qualified-name>(<canonical-argument-1>, ...)
```

It adds distinct `New`, `ConstructorSelect`, and `TypeSelect` plan kinds because
their raw node identities and point conventions differ from ordinary term
selection. The positioner recursively maps the direct raw tree under one
virtual `SourceFile`. Tests independently parse the generated spelling and
compare every relevant node's runtime kind, child order, name/arity detail,
start, point, end, and source slice. Production never reparses generated text.

Definition raw and generated-origin backends inherit this support through the
existing term-body seam; no definition syntax, owner, or placement policy was
added.

## Malformed and negative boundary

Hostile/manual values return controlled internal errors for null, simple, or
malformed names; null argument lists/elements; unsupported nested nodes; and
missing/unconsumed type sidecars. Phase 71 frontend exclusions remain intact:
simple/imported names, generic or path-dependent constructors, constructor
holes, backticked or binary segments, multiple/named/using argument lists, and
anonymous templates are unsupported. Phase 74 lambda/block production syntax
remains absent.

## API, artifacts, and validation

The Phase 73 check reports `NO_PUBLIC_API_DELTA`: 575 rows, core 284 and
frontend 291. `core` remains compiler-free, frontend coordinates remain
full-compiler-version coupled, and root plus `dottyInternal` remain
unpublished. Four affected compiler lanes, full stable/3.3.8/3.9.0-RC1 gates,
the pinned-nightly backend/package lane, three fresh stable JVM passes, static
source scans, API diff, and a disposable public/control split rehearsal form
the Phase 75 evidence. Local results do not imply hosted CI or release.

The stable closure totals are core 134, frontend 672, `dottyInternal` 192,
public-core examples 10, and public-API examples 5 in each of the three
consecutive runs. Two retained evidence repairs did not change production
scope: the parser oracle stays inside one exact compiler test universe, and
the sanitized public overlay contains only self-contained public documents;
the split residual and link audit rules were not weakened.

The exact next action is **Phase 76 — Conditional Authorized Split/Release
Execution Or Reproducible Local Release Bundle Refresh**. No Phase 76 external
action is authorized by Phase 75.
