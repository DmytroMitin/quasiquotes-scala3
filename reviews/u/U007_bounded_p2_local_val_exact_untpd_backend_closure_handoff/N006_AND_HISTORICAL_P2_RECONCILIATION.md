# N006 and historical P2 reconciliation

N006 already projects the bounded Scalameta source:

```scala
{ val x: Int = 1; x }
```

to `TermShape.Block(List(BlockStatement.LocalVal(...)), BoundReference(...))` with a shared `BinderId`, initializer-old-scope semantics, and a declared-type display string.

U006 correctly rejected that node in every exact backend because P1 was its authorized scope. U007 changes only the richer `ConstructedTerm` routes, where completed `TypeNormalForm` sidecars are authoritative. The historical U006 tests were updated to preserve direct rejection while expecting richer/generated acceptance.

No N006 projector or Core semantic code changed. This is semantic-copy lowering; it does not preserve Scalameta node identity or an existing raw subtree identity.
