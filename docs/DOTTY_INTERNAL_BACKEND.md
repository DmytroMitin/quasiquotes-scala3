# Dotty-internal exact backend

`dottyInternal` is an experimental, remotely unpublished, full-crossed module
for exact Scala compiler operations. It is not a generic public `untpd` or
`tpd` toolkit. Consumers must align with the module's full Scala compiler
version and active compiler context.

## Current public-for-JVM-access surface

`quasiquotes.definitions.dotty.ContextualMethodPeerBridge` is the only
production object in this module intentionally exposed to a foreign package.
It accepts one admitted Scalameta `Defn.Def` and a virtual source name. Its
result contains a positioned `untpd.DefDef`, deterministic generated source,
and the effective virtual source name, or a categorized failure.

The real consumed path is:

```text
scala.meta.Defn.Def
  -> ScalametaContextualMethodProjection
  -> validated project/core DefinitionResultView
  -> PublicContextualMethodUntypedBackend
  -> PublicContextualMethodGeneratedOriginAdapter
  -> ContextualMethodPeerBridge.Lowered
       -> positioned untpd.DefDef
       -> generated source
       -> virtual source name
```

The bridge is used by the narrow AUXify/Macro-Paradise integration. AUXify
authors and requests the contextual method, while Macro-Paradise owns plugin
lifecycle, companion placement and merge, insertion, rollback, and ordinary
typing. Macro-Paradise itself has no Quasiquotes or Scalameta product
dependency.

The focused API and failure contract remain documented on the
[experimental contextual-method peer bridge page](EXPERIMENTAL_CONTEXTUAL_METHOD_PEER_BRIDGE.md).

## Internal module inventory

All other production owners are package-private or otherwise project-internal:

- `ConstructedTermUntypedBackend` and `CompletedTypeUntypedLowerer` lower the
  admitted compiler-free Term/Type models to source-free raw trees.
- `ConstructedDefinitionUntypedBackend` and
  `PublicContextualMethodUntypedBackend` construct bounded raw definitions.
- `ScalametaContextualMethodBackend` is an internal bounded forward/reverse
  adapter for the contextual-method shape.
- the generated-origin adapters, result carriers, fragment planner,
  interpolation encoder, and position validators create deterministic virtual
  source and complete truthful spans for admitted generated trees.
- the error ADTs keep internal failure boundaries deterministic without
  widening the foreign-package bridge.

`ConstructedDefinitionGeneratedOriginAdapter`, its single- and two-parameter
specializations, and the public-contextual-method generated-origin adapter
combine raw shapes with planned source provenance. The large
`GeneratedOriginFragmentSupport` owner performs the reusable term/type fragment
rendering, structural planning, positioning, and validation.

## Test-only exact-compiler evidence

Tests demonstrate two capabilities that are not production APIs:

- current `q.reflect.Term` values are implemented by `tpd.Tree`, and
  `tpd.applyOverloaded` can construct the typed addition on the exact compiler
  lines;
- `untpd.TypedSplice` can embed typed leaves in a newly constructed untyped
  shell, after which `Typer.typedExpr` produces a typed result.

These probes do not authorize manual `ExprImpl` construction, a general
`tpd.Tree -> untpd.Tree` inverse, or a stable bridge between arbitrary compiler
contexts.

## Deliberate exclusions

There is currently no production public bridge from arbitrary
`scala.meta.Term` to `untpd.Tree`, no public neutral projector from arbitrary
`scala.meta.Term` to a core Term representation, no generic raw-tree family,
no placement service, and no stable published coordinate for this module.
Typed Scalameta Term traversal instead belongs to the separate unpublished
`hybridScalametaFrontend` and returns caller-owned `q.reflect.Term`.

Future work may add a compiler-free bounded `scala.meta.Term -> TermShape`
projector if a concrete reusable consumer appears. A one-operation exact peer
bridge could then compose that neutral projection with internal exact lowering.
Neither is required for the next source-owned local-`def` product gate or the
independent AUXify 037 Type/Definition gate.
