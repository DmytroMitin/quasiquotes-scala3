# Contributing

This repository is experimental and is not yet accepting a stable compatibility
commitment. Discuss substantial API, syntax, module, or compatibility changes
before implementation.

Contributions cannot be accepted for public redistribution until the repository
has an explicitly selected license and contribution terms. This document
describes the intended technical workflow; it does not grant rights absent a
license.

## Development baseline

- JDK 25
- sbt 1.12.8
- required Scala baseline 3.8.4

Run the complete local gate before proposing a change:

```sh
sbt -batch clean test publicCoreExamples/test publicApiExamples/test \
  core/verifyCoreBoundary verifyModuleGraph package
git diff --check
```

Changes must keep `core` free of `scala.quoted` and Dotty compiler dependencies,
retain full-version crossing for `frontend`, and leave the root and
`dottyInternal` artifacts unpublished. Update user documentation when behavior,
dependencies, build steps, module responsibilities, or compatibility policy
changes.

Do not include credentials, private repository links, machine-local paths, or
generated build output.

Do not report sensitive vulnerabilities in a public issue. The unresolved
private reporting-channel gate is documented in [SECURITY.md](SECURITY.md).
