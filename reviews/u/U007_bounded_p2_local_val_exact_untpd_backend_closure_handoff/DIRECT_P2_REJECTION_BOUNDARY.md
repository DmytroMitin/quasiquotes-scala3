# Direct P2 rejection boundary

`CoreTermShapeUntypedLowerer` was not modified.

It still receives only `TermShape`/`BlockStatement` syntax and no authoritative completed `TypeNormalForm` stream. Consequently a direct Block containing `LocalVal` remains controlled malformed/unsupported Block input. It does not parse the declared-type display string and does not fall back to the richer backend.

The existing direct P1 suite remains green on all three compiler lines, including ordered prefix/result topology, nested P1, source-free invariants, and direct/richer overlap.

```text
DIRECT_P2 = REJECTION_PRESERVED
```
