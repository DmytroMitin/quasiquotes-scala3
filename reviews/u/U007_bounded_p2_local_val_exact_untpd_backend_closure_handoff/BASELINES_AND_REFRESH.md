# Baselines and refresh

U007 launched from clean synchronized product `c2cf6ffa752fa9e70be6fcbccc51a7b8d9f9c58a` and control `60901df4ee52ef3655d666e8df036c1b762d5b0c` after fetching both `main` branches.

The post-prompt product movement was N007 only: bounded Scalameta local-def/P3 projection. It touched neutral projection/tests, not U exact backend owners. U007 therefore treated it as future P3 context and retained exact `LocalDef` rejection.

The final refresh found:

```text
product HEAD == origin/main == c2cf6ffa752fa9e70be6fcbccc51a7b8d9f9c58a
product divergence: 0 0
control local HEAD: 60901df4ee52ef3655d666e8df036c1b762d5b0c
control origin/main: 6e93e048c785d10f2a690ce3708a7fd84ceb2417
control divergence: 0 4
```

The four control-only commits accepted N007, reported its parity update, added N008, and selected it. They changed no product/build/module topology, so no reconciled product rerun was required. The already-completed three-line full module matrix remains applicable.
