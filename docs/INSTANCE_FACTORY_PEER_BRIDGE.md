# Experimental instance-factory peer bridge

`quasiquotes.definitions.dotty.InstanceFactoryPeerBridge` is an exact-Scala-
version entry point in the remotely unpublished `dottyInternal` artifact. It
accepts one complete Scalameta `Defn.Def` in this bounded semantic family:

```scala
def instance[A](
  emptyValue: => A,
  combineFunction: (A, A) => A
): Monoid[A] =
  new Monoid[A]:
    override def empty: A = emptyValue
    override def combine(a: A, a1: A): A =
      combineFunction(a, a1)
```

All source names may be coherently renamed. The structure remains exact: one
unbounded invariant type parameter, one ordinary two-parameter clause, a
by-name first carrier, a structural binary-function second carrier, one direct
unary result and matching anonymous parent, and exactly two ordered override
members. The empty body must reference the first outer carrier. The combine
callee and its two ordered arguments must resolve to the exact outer function
carrier and nested parameter binders.

```scala
InstanceFactoryPeerBridge.lower(definition, virtualSourceName)
```

returns either a categorized `Failure` or a `Lowered` value containing one
positioned insertion-ready `untpd.DefDef`, its deterministic generated source,
and the effective virtual source name. The internal semantic plan and its
projector/lowerer error types are not part of the public boundary.

The implementation composes existing authorities mechanically:

```text
scala.meta.Defn.Def
  -> exact compiler-free factory projection
  -> package-private binder-aware factory plan
  -> source-free exact raw lowering
  -> deterministic generated-origin positioning
  -> positioned untpd.DefDef
```

The projector owns the complete source topology, lexical scope, collision,
Type-role, and Term-reference rules. The exact backend owns compiler-line raw
shape, source-free invariants, rendering, positions, and generated provenance.
The bridge maps both boundaries into stable categories for missing input,
unsupported topology, invalid names, Type-role failures, Term/binder-role
failures, invalid virtual-source input, exact lowering, generated origin, and
internal invariants. It has no permissive fallback and returns no partial
factory.

Every generated nonempty node belongs to the same virtual source, has a
contained deterministic span, remains `NoSymbol`, and contains no
`TypedSplice` before ordinary typing. The result is tested for pre-Typer
insertion, class and TASTy emission, and runtime behavior on Scala 3.3.8,
3.8.4, and 3.9.0. The standard runtime control yields `empty == 7` and
`combine(20, 22) == 42`.

The caller retains target admission, companion creation or merge, insertion,
conflict policy, rollback, and typing. Source inspection and Scalameta
authoring likewise remain consumer responsibilities. This bridge does not
implement an annotation lifecycle and is not a general `Defn.Def`, anonymous-
class, template, or raw-tree API.
