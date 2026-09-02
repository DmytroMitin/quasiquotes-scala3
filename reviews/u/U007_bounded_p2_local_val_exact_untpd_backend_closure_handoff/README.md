# U007 bounded P2 local-val exact untyped backend closure

Decision:

```text
U007_COMPLETE_BOUNDED_P2_LOCAL_VAL_EXACT_BACKEND_CLOSURE
```

The richer `ConstructedTermUntypedBackend` now lowers the already-admitted one-P2 `LocalVal` using completed type sidecars. `GeneratedOriginFragmentSupport` renders and positions the matching parser-equivalent `ValDef`. The direct `CoreTermShapeUntypedLowerer` remains closed because it has no authoritative completed type sidecar.

Start/final observed identities:

```text
product start HEAD/main: c2cf6ffa752fa9e70be6fcbccc51a7b8d9f9c58a
product final HEAD/origin-main: c2cf6ffa752fa9e70be6fcbccc51a7b8d9f9c58a
control start local HEAD/origin-main: 60901df4ee52ef3655d666e8df036c1b762d5b0c
control final local HEAD: 60901df4ee52ef3655d666e8df036c1b762d5b0c
control final live origin-main observed: 6e93e048c785d10f2a690ce3708a7fd84ceb2417
prompt sha256: 45176e2eb33abf6e9413c1fe1c6de4d75db9f910dbccbff57a1cf0f6551fad40
```

All changes and evidence remain unstaged and uncommitted. No push, PR, tag, release, public documentation, shared Core production, Q/N production, build, or peer-repository mutation was performed.

Primary status:

```text
DIRECT_P2 = REJECTION_PRESERVED
RICHER_SOURCE_FREE_P2 = PASS
GENERATED_ORIGIN_P2 = PASS
N006_NEUTRAL_TO_EXACT = PASS
P2_SCOPE = PASS
P3_BOUNDARY = PRESERVED
PRE_TYPER = PASS_ALL_THREE
PUBLIC_API_DELTA = NONE
SHARED_CORE_IMPACT = NONE
D_U_SEPARATION = PRESERVED
PUBLIC_U_SYNTAX = NONE
```
