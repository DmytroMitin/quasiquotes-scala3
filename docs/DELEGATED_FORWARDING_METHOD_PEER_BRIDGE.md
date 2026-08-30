# Experimental delegated forwarding-method peer bridge

`DelegatedForwardingMethodPeerBridge` is an exact-version entry point in the
remotely unpublished `dottyInternal` module. It accepts only an already-authored
Scalameta method with this topology:

```scala
def show[A](a: A)(using inst: Show[A]): String = inst.show(a)
```

All six names may be coherently renamed. The method still has exactly one
unbounded type parameter, one ordinary parameter, one final contextual
parameter, one direct named result type, and one selected one-argument
forwarding body.

```scala
DelegatedForwardingMethodPeerBridge.lower(definition, virtualSourceName)
```

returns either a categorized `Failure` or a `Lowered` carrier containing the
positioned `untpd.DefDef`, deterministic generated source, and effective
virtual source name. The compiler-free plan distinguishes the type, ordinary
term, and contextual term binders by identity. It also uses one method identity
for both the declaration and selected body member. Display spelling is retained
for generated source and diagnostics; it is not used as a substitute for
binder identity or semantic name resolution.

The production route validates the exact Scalameta shape, constructs the raw
tree directly without rendering and reparsing, then assigns complete positions
inside one generated virtual source. Before ordinary compiler typing, every
nonempty node has that generated source and a valid span, remains `NoSymbol`,
and contains no `TypedSplice`.

The caller continues to own target admission, derivation, companion creation
or merge, placement, conflict policy, insertion, rollback, and typing. The
bridge neither inspects a trait nor claims that AUXify's canonical `@delegated`
handler is integrated. It does not admit bounded parameters, additional
clauses, defaults, by-name or repeated parameters, arbitrary bodies, or an
arbitrary `Defn.Def -> untpd.DefDef` conversion.

The failure codes preserve the first meaningful boundary, including definition,
type-parameter, clause, parameter, applied-type, result, body, binder, selected
member, generated-origin, raw-lowering, and genuine internal-invariant failures.
