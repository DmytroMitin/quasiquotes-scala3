# Source-free richer P2 lowering

`ConstructedTermUntypedBackend` now performs a small block-statement state transition:

```text
consume completed declared-type sidecar
lower it with CompletedTypeUntypedLowerer
lower initializer under the incoming binder map
construct untpd.ValDef with EmptyFlags
extend BinderId -> validated declaration spelling
lower later children/result
restore the incoming binder map at Block exit
retain the final typed ordinal
```

The produced canonical raw tree matches the parser oracle. Every authored node remains `NoSource`, span-free, `NoSymbol`, and contains no `TypedSplice`. No compiler symbol, owner, placement, or typechecking claim is manufactured.

The lowering state now tracks whether a Lambda1 is active separately from the binder-name map. It also records whether the lowering entry point supplied ambient binders. Together these preserve the one-Lambda restriction and the existing definition-body Lambda boundary while allowing the already-admitted distinct-name Lambda1/P2 nesting in either direction.
