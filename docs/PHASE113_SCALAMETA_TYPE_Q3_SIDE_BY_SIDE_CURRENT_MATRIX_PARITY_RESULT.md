# Scalameta Type-Q3 side-by-side parity result

This gate implements a private Scalameta-primary Type frontend beside the
current public `tqr`/`tqq` implementation. It completes the current bounded
public matrix without changing any public default or API.

## Result

```text
TYPE_Q3_CURRENT_PUBLIC_MATRIX_PARITY_COMPLETE
TYPE_Q3_READY_FOR_SEPARATE_SCALAMETA_OPT_IN_API_GATE
PUBLIC_TQR_TQQ_DEFAULT = CURRENT_DOTTY
PUBLIC_QR_QQ_DEFAULT = CURRENT_DOTTY
CURRENT_DOTTY_ENGINE_RETIREMENT = NOT_AUTHORIZED
PUBLIC_SCALAMETA_TYPE_OPT_IN_API = NOT_AUTHORIZED_IN_PHASE113
CI_NOT_REACHED
```

The executable inventory has 30 unique rows: 21 current public Type cases,
three cases that are not public Type contracts, and six explicitly deferred
families. The admitted slice covers named scalar types, recursive fixed
`List`/`Option`/`Either`, Tuple2/Tuple3, Function1/Function2, programmatic
template/pattern holes and repeated-hole equality, interpolated reflected
construction, reflected matching, mismatch fallthrough, and exact original
capture identity.

Selected/path-dependent types, broader constructors/arities, wildcards and
bounds, refinements, match types, unions/intersections, and other advanced
types remain outside the gate.

## Architecture and fallback

Scalameta 4.17.3 parses a public `scala.meta.Type`. A project-owned mapper
converts that tree directly to the existing `TypeShape`; existing
`TypeNormalForm`, `TypeTemplate`, `TypePattern`, `TypeReprLowerer`, and
`TargetTypeReprInspector` retain all semantic authority. No Scalameta tree is
printed and reparsed as a normal bridge, and no second Type semantic model is
introduced.

The active compiler line chooses Scalameta `Scala3` for Scala 3.3.8 and
`Scala38` for Scala 3.8.4. Accepted Scalameta source must also pass the current
exact-compiler parser. Only `SCALAMETA_PARSE_FAILURE` may fall back to the
current frontend. Exact-compiler rejection, unsupported mapped syntax, splice
inspection failure, target inspection failure, and construction failure are
terminal and categorized.

## Evidence boundary

The focused 11-test Type-Q3 suite and the full aggregate pass on Scala 3.3.8
and 3.8.4. A fresh independent linked consumer in a disposable project passes
on both lines and proves direct mapping plus compiler-free template/pattern
semantics through the private module seam. Reflected construction and exact
capture identity remain covered by the in-repository two-line staging tests.

The implementation and inventory remain `private[quasiquotes]` inside the
remotely unpublished full-cross `hybridScalametaFrontend` module. Released
`core` and `frontend` production sources are unchanged and retain their exact
618-row public API inventory. Direct Scalameta Type authoring is already the
neutral syntax; this gate does not manufacture `ntqr`/`ntqq` or move
compiler-coupled Type lowering into `neutralScalameta`.
