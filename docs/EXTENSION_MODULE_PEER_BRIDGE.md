# Bounded extension-module peer bridge

Active `0.4.0-SNAPSHOT` source contains one public-for-JVM exact-version
composition for the AUXify input045 first slice:

```scala
import quasiquotes.definitions.dotty.ExtensionModulePeerBridge

ExtensionModulePeerBridge.lower(definition, "GeneratedSyntax.scala")
```

The input is one `scala.meta.Defn.Object` with exactly one extension group,
one Type parameter and receiver, and one nested method with one ordinary
parameter followed by one final `using` parameter. The body is exactly the
contextual evidence's same-named method applied to the receiver and ordinary
argument. All seven source-role names are validated and preserved.

The implementation composes the existing bounded authorities without adding a
second grammar or raw backend:

```text
Defn.Object
  -> ScalametaExtensionModuleProjection
  -> ExtensionModulePlan
  -> bounded plan adapter
  -> BoundedExtensionModuleGeneratedOriginAdapter
  -> positioned untpd.ModuleDef
```

Success returns the `ModuleDef`, deterministic generated source, and effective
virtual source name. Failures use stable outer categories for missing input,
unsupported topology, invalid name/Type/Term roles, invalid virtual source,
exact/generated lowering, and internal invariants; internal error carriers
do not cross the public boundary.

This is not general object or extension lowering. It does not own annotation
admission, companion placement or merging, plugin lifecycle, insertion,
rollback, typing, symbols, or owner repair. Those operations remain consumer
responsibilities. In particular, the bridge being available does not mean the
real AUXify `@syntax` vertical is integrated.
