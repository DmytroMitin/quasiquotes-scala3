# Sidecar preorder contract

Live `TermShapeTraversal.typedNames` establishes this order for P2:

```text
1. LocalVal declared type
2. initializer typed descendants in term preorder
3. later result-block prefix typed/Lambda descendants in source order
4. final result typed/Lambda descendants
```

The focused exact test uses:

```text
LocalVal declared type: List[Int]
initializer Typed: String
later result-block prefix Typed: Boolean
result Lambda1 parameter: Int
```

and supplies exactly:

```text
Vector(List[Int], String, Boolean, Int)
```

Raw type-tree inspection proves each sidecar reaches the intended node. No declared-type display string is parsed. Corruption tests prove missing ordinal 0, unsupported ordinal 0, and one extra unconsumed sidecar produce the existing bounded private errors. `ConstructedTerm.create` continues rejecting count and rendering mismatches before backend entry.
