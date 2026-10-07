# Experimental instance-factory peer bridge

`quasiquotes.definitions.dotty.InstanceFactoryPeerBridge` is an exact-Scala-
version entry point in the `dottyInternal` artifact. Its single public
`lower` operation accepts one complete Scalameta `Defn.Def` from exactly
four closed semantic families.

The two-method family has a by-name value carrier, a binary-function carrier,
and two ordered method overrides:

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

The one-value family has one strict carrier and one immutable override:

```scala
def instance[A](valueValue: A): HasValue[A] =
  new HasValue[A]:
    override val value: A = valueValue
```

The Type-member family has two Type parameters, no value clause, and the same
single concrete Type equality in its result refinement and anonymous body:

```scala
def instance[A, Out0]: HasOut[A] { type Out = Out0 } =
  new HasOut[A]:
    type Out = Out0
```

The curried-method family has one nested-unary function carrier and one method
override with two ordinary one-parameter clauses:

```scala
def instance[A](combineFunction: A => A => A): Curried[A] =
  new Curried[A]:
    override def combine(a: A)(b: A): A = combineFunction(a)(b)
```

All source names may be coherently renamed. Each family keeps its exact
parameter, result, parent, member, binder-reference, and lexical-safety rules.
The one-value carrier and member names must remain distinct so the initializer
references the outer carrier. The Type-member alias name and second Type
parameter must also remain distinct so the equality is not cyclic.

```scala
InstanceFactoryPeerBridge.lower(definition, virtualSourceName)
```

returns either a categorized `Failure` or a `Lowered` value containing one
positioned insertion-ready `untpd.DefDef`, its deterministic generated source,
and the effective virtual source name. The public operation and result/error
carriers are unchanged; private family selection, semantic plans, projectors,
lowerers, and their private error types are not part of the public boundary.

The existing two-method projector runs first. Only after it fails do ordered
shallow, structural, mutually exclusive envelopes select one of three sibling families;
the selected complete projector remains authoritative for topology, lexical
scope, Type roles, and binder references. If no sibling envelope matches,
the exact original two-method public failure is returned. The curried envelope is
ordered after the one-value and Type-member envelopes.

Each accepted private path then performs source-free exact raw lowering and
deterministic generated-origin positioning. The bridge maps those boundaries
into the same stable public categories for missing input, unsupported topology,
invalid names, Type-role failures, actual Term/binder-role failures, invalid
virtual-source input, exact lowering, generated origin, and internal
invariants. It has no permissive fallback and returns no partial factory.

Every generated nonempty node belongs to the same virtual source, has a
contained deterministic span, remains `NoSymbol`, and contains no
`TypedSplice` before ordinary typing. The four families are exercised
together through the same public bridge for pre-Typer insertion, class and
TASTy emission, runtime behavior, and Type-member refinement use across the
supported Scala compiler lines.

The caller retains target admission, companion creation or merge, insertion,
conflict policy, rollback, and typing. Source inspection and Scalameta
authoring likewise remain consumer responsibilities. This bridge does not
implement an annotation lifecycle and is not a generic `Defn.Def`, anonymous-
class, template, or raw-tree API.
