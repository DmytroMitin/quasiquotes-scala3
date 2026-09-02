# N006 neutral-to-exact composition

The production overlap is proven in `P2LocalValExactBackendTest`:

```text
scala.meta.Term.Block
-> ScalametaTermProjection
-> TermShape.Block / BlockStatement.LocalVal
-> ConstructedTerm.fromShape with derived Int sidecar
-> ConstructedTermUntypedBackend
-> exact source-free Block(ValDef, result)
```

The same `ConstructedTerm` independently reaches `ConstructedTermGeneratedOriginAdapter` and a fully positioned parser-equivalent P2 tree.

The richer `List[Int]` declared-type case uses `ConstructedTerm.create` with the completed `STypeApply(List, Int)` sidecar, proving that the display text is not authoritative.
