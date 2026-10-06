# Experimental self abstract-Type-member peer bridge

`quasiquotes.definitions.dotty.SelfAbstractTypeMemberPeerBridge` is a narrow,
exact-Scala-version entry point in the current `dottyInternal` development
surface. One unchanged `lower` operation admits exactly the closed matrix formed
by two independent structural edges:

```scala
type Self >: self.type <: Nat { type Self = self.Self } // both present
type Self >: self.type <: Nat                           // lower only
type Self <: Nat { type Self = self.Self }              // F-bound only
type Self <: Nat                                        // neither
```

The member, prepared self alias, and upper base remain required explicit
expectations for every row, so coherently renamed forms use the same operation.
The outer member and direct or refined upper base must match their expectations.
When present, the lower singleton and refinement selection must use the expected
self alias; even in the neither row, that expectation is still validated as a
legal prepared-self source role.

Absence is structural. A missing lower edge lowers to `untpd.EmptyTree` inside
the `TypeBoundsTree`, without synthesizing `scala.Nothing` or source text. A
missing F-bound edge lowers to the direct upper-base `Ident`, without an empty
`RefinedTypeTree` or placeholder source. The resulting nonempty raw-node counts
are respectively 9, 5, 7, and 3.

This is not arbitrary `TypeDef` or `TypeBounds` lowering. The declaration must
be unmodified and non-generic, with a required named upper base. Any present
lower must be one direct singleton. Any present F-bound must be one named-base
refinement containing exactly one unmodified, non-generic Type alias with no
auxiliary bounds and one direct selected-Type RHS. Malformed present edges,
extra members, paths, applications, unions, intersections, context/view bounds,
or other definition kinds fail closed.

The self alias is peer-prepared external syntax. Quasiquotes validates an
ordinary stable name or the bounded collision form `base$N`, where `N` is a
positive decimal without a leading zero. It does not allocate a `BinderId`,
create a compiler symbol, search an ambient scope, choose alias freshness, or
rewrite a trait.

The ownership split is deliberate:

- AUXify owns `@self` semantics and the neutral Scalameta declaration;
- Macro-Paradise owns primary-trait admission, self-alias preparation,
  insertion, rollback, and ordinary typing;
- Quasiquotes owns exact matrix validation, source-free `untpd.TypeDef`
  lowering, and deterministic generated-source positioning.

On success, `lower` returns a `Lowered` value exposing only the positioned
`untpd.TypeDef`, generated source, and effective virtual source name. Every raw
node begins without source, span, symbol, or typed splice; the generated-origin
adapter then positions every nonempty node under one fresh virtual source while
retaining `NoSymbol` and no `TypedSplice`. Failures expose compact `code` and
`detail` fields and do not fall back to a permissive projector.

Product fixtures cover Scala 3.3.8, 3.8.4, and 3.9.0. The active development
version is `0.4.0-SNAPSHOT`; this expanded matrix is not presented as a released
coordinate. The sibling
[contextual-method bridge](EXPERIMENTAL_CONTEXTUAL_METHOD_PEER_BRIDGE.md)
continues to own the legacy and bounded `Add.Out` `untpd.DefDef` families.
