# Parser raw P2 oracle

`P2LocalValRawCharacterizationTest` characterizes all required sources plus the distinct-name P2/Lambda composition on Scala 3.3.8, 3.8.4, and 3.9.0-RC1.

For canonical `{ val x: Int = 1; x }`, all three lines agree:

```text
raw structure: Block([ValDef], Ident(x))
Block stats: one leading untpd.ValDef
ValDef name: x
ValDef modifiers: Flags.EmptyFlags
ValDef type: Ident(Int)
ValDef initializer: Number(1, Whole(10))
Block result: Ident(x)
```

Exact spans/points are also identical:

```text
Block       0..0..21
ValDef      2..6..16
type        9..9..12
initializer 15..15..16
result      18..18..19
```

The remaining required fixtures are asserted with the same full-tree precision: exact Block/ValDef/type/initializer/later-stat/result topology, exact node variants and values, and exact spans/points. This includes `String`, `Boolean` plus `if`, a later `f(x)` statement plus `x + 1` result, and the distinct-name Lambda1 composition. The parser places the Lambda body expression inside an empty P0 `Block`; the oracle records that structure rather than normalizing it away. No line-specific raw constructor, modifier, topology, or span branch was required.
