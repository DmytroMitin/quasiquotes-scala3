# Validation

Red evidence on Scala 3.8.4: the initial focused U007 suite compiled and failed 7/7 because existing richer/generated paths rejected `LocalVal`; this was the intended missing behavior.

Parser characterization:

```text
P2LocalValRawCharacterizationTest: 6/6 on 3.3.8, 3.8.4, 3.9.0-RC1
```

Focused cross-line selection:

```text
pre-review broad selection: 84/84 on each compiler line
post-review exact selection: 39/39 on each compiler line
```

The post-review selection included the exact P2 raw/backend/runtime suites plus the Lambda1 and definition-body boundary suites affected by the scope-provenance correction.

Complete module regression:

```text
sbt -batch '++3.3.8!' 'dottyInternal/test' \
  '++3.8.4!' 'dottyInternal/test' \
  '++3.9.0-RC1!' 'dottyInternal/test'

3.3.8: 458/458
3.8.4: 458/458
3.9.0-RC1: 458/458
```

The matrix exited successfully after review strengthening. `P2LocalValExactBackendTest` passed 9/9 on every line. The additions cover both lowering paths' scope restoration, same-text free/reference identity, exact generated sidecar preorder, direct LocalDef rejection, an actual P2 raw/plan mismatch, and preservation of the ambient definition-body Lambda boundary. Expected negative-probe compiler diagnostics and pre-existing P1 pure-expression warnings occurred inside passing tests.
