# Generated-origin P2 contract

`GeneratedOriginFragmentSupport` adds one private `LocalVal` node-plan kind. Rendering is deterministic:

```scala
{ val x: Int = 1; x }
```

It consumes the declared-type sidecar before the initializer, renders the initializer with the old binder environment, installs the local `BinderId` spelling only afterward, and restores the incoming environment when leaving the Block. Typed-ordinal progression is not restored.

The positioner consumes a raw `untpd.ValDef` plus exactly two plans: declared type and initializer. It reconstructs the ValDef with its existing modifiers and attaches the ValDef plan span. The generated canonical spans exactly equal the parser oracle:

```text
Block 0..0..21; ValDef 2..6..16; type 9..9..12;
initializer 15..15..16; result 18..18..19
```

Every material node has the same virtual source, an existing contained span, and `NoSymbol`. No generic statement renderer or P3 plan was introduced.
