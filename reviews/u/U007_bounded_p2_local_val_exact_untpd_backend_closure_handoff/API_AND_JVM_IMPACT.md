# API and JVM impact

Standard public API delta is none. No public source module, public syntax, build definition, artifact coordinate, version, or release metadata changed.

Package-private exact-version implementation impact:

- `ConstructedTermUntypedBackend` gains private P2 block/local-val state transitions plus private Lambda-active and ambient-binder provenance state;
- `GeneratedOriginFragmentSupport` gains private `NodeKind.LocalVal`, local-val rendering/positioning, plus private Lambda-active and ambient-binder provenance state;
- two package-private error message prefixes now describe bounded P1/P2 Blocks.

All affected production declarations are inside existing `private[quasiquotes]` exact-version internals. There is no public JVM signature or public TASTy surface change.

```text
PUBLIC_API_DELTA = NONE
```
