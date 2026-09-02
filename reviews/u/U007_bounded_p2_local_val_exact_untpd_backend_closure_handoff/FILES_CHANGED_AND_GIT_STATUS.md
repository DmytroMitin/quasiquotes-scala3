# Files changed and git status

Production:

```text
dotty-internal/src/main/scala/quasiquotes/terms/dotty/ConstructedTermUntypedBackend.scala
dotty-internal/src/main/scala/quasiquotes/terms/dotty/GeneratedOriginFragmentSupport.scala
dotty-internal/src/main/scala/quasiquotes/terms/dotty/ConstructedTermUntypedBackendError.scala
dotty-internal/src/main/scala/quasiquotes/terms/dotty/ConstructedTermGeneratedOriginError.scala
```

Tests:

```text
dotty-internal/src/test/scala/quasiquotes/terms/dotty/P2LocalValRawCharacterizationTest.scala
dotty-internal/src/test/scala/quasiquotes/terms/dotty/P2LocalValExactBackendTest.scala
dotty-internal/src/test/scala/quasiquotes/terms/dotty/ConstructedTermGeneratedOriginTyperRuntimeTest.scala
dotty-internal/src/test/scala/quasiquotes/terms/dotty/ConstructedTermUntypedBackendTest.scala
dotty-internal/src/test/scala/quasiquotes/terms/dotty/P1BlockExactBackendTest.scala
```

Evidence is this 19-file handoff directory. `git diff --check` passes. The product tree is intentionally dirty only with U007 production/tests/evidence, and everything is unstaged. Control has no U007 working-tree edit. No commit or push was authorized.
