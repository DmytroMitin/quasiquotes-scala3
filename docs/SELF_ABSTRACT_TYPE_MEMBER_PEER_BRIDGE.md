# Experimental self abstract-Type-member peer bridge

`quasiquotes.definitions.dotty.SelfAbstractTypeMemberPeerBridge` is a narrow,
exact-Scala-version entry point in the remotely unpublished `dottyInternal`
artifact. It lowers one bounded Scalameta `Decl.Type` family:

```scala
type Self >: self.type <: Nat { type Self = self.Self }
```

The member, prepared self alias, and upper base are explicit expectations, so
renamed coherent forms such as the following use the same operation:

```scala
type Element >: owner$2.type <: Domain {
  type Element = owner$2.Element
}
```

This is not arbitrary TypeDef lowering. The declaration must be unmodified and
non-generic, have one direct singleton lower bound, and have one named refined
upper bound containing exactly one unmodified Type alias. The outer member,
refinement alias, selected prefix, and selected member must agree with the
explicit expectations. Extra bounds, members, paths, applications, unions,
intersections, or other definition kinds fail closed.

The self alias is peer-prepared external syntax. Quasiquotes validates an
ordinary stable name or the bounded collision form `base$N`, where `N` is a
positive decimal without a leading zero. It does not allocate a `BinderId`,
create a compiler symbol, search an ambient scope, choose alias freshness, or
rewrite a trait.

The ownership split is deliberate:

- AUXify owns `@self` semantics and the neutral Scalameta declaration;
- Macro-Paradise owns primary-trait admission, self-alias preparation,
  insertion, rollback, and ordinary typing;
- Quasiquotes owns exact structural validation, source-free nine-node
  `untpd.TypeDef` lowering, and deterministic generated-source positioning.

On success, `lower` returns a `Lowered` value exposing only the positioned
`untpd.TypeDef`, generated source, and effective virtual source name. Every raw
node begins without source, span, symbol, or typed splice; the generated-origin
adapter then positions all nine nodes under one virtual source while retaining
`NoSymbol` and no `TypedSplice`. Failures expose compact `code` and `detail`
fields and do not fall back to a more permissive projector.

Product fixtures cover Scala 3.3.8, 3.8.4, and final 3.9.0. The disposable live
AUXify coordinate proof and source-built Macro-Paradise callback proof use
Scala 3.8.4. The Macro-Paradise callback is source-built and unreleased; no
stable published coordinate for this integration is claimed. The sibling
[contextual-method bridge](EXPERIMENTAL_CONTEXTUAL_METHOD_PEER_BRIDGE.md)
continues to own the legacy and bounded `Add.Out` `untpd.DefDef` families.
