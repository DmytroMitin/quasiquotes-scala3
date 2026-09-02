# Fail-closed matrix

Controlled evidence covers:

```text
null ConstructedTerm                         existing missing-term error
null TermShape/result/child                  existing bounded errors
direct LocalVal                              direct Block rejection preserved
LocalDef/P3                                  richer and generated Block rejection
missing declared-type sidecar                MissingTypeSidecar(0)
extra sidecar                                UnconsumedTypeSidecars(1, 2)
unsupported completed type                   UnsupportedTypeSidecar(0, ...)
out-of-scope BoundReference                  OutOfScopeBoundReference
self-reference from initializer              OutOfScopeBoundReference
malformed local declaration spelling         controlled private name error
second/nested P2                             existing Core admission rejection
same-name P2/Lambda                          existing Core admission rejection
unsupported child                            existing bounded node error
generated raw/plan mismatch                  exercised with an actual P2 fragment/raw-tree mismatch
```

No ordinary negative route relies on a compiler assertion or exception as protocol.
