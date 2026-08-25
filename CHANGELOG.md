# Changelog

All notable user-visible changes will be recorded here. Entries are grouped
under `Added`, `Changed`, `Fixed`, `Deprecated`, `Removed`, and `Security` where
applicable. Dates and release links are added only when a release exists.

## Unreleased

### Added

- Apache License 2.0 project, POM, and binary/source/documentation JAR metadata
  for the intended `core` and `frontend` distributions.
- Compiler-free structural values, construction, matching, normal forms,
  source metadata, and a bounded public contextual-method description.
- Compiler-coupled source parsing, quoted lowering, diagnostics, term
  construction/matching, and typed type integration.
- Recursive `List` and `Option` plus binary `Either` type structures across
  source, matching, construction, TypeRepr, and scoped type evidence.
- Bounded structural standard-`s` interpolation support.
- Package-private compiler-free one-ordinary-parameter definition shapes,
  alpha-aware templates/completion, and explicit source/backend deferment
  boundaries.
- External-package core and frontend examples and deterministic build-boundary
  checks.
- An unpublished binary-crossed `neutralScalameta` experiment using Scalameta
  4.17.3 for direct term/type/definition construction and matching, plus a
  bounded structural contextual-method projection into the existing validated
  IR.
- An unpublished exact-backend bridge for the admitted contextual method in
  both directions, including generated/no-position reverse matching without
  print/reparse.

### Changed

- `0.2.0` is the immutable Maven Central release. The active source tree is the
  unpublished `0.3.0-SNAPSHOT` development line.
- The build uses sbt 1.12.15 and sbt-pgp 2.3.1 for manual, local-only signed
  staging with fail-closed public developer metadata.
- Experimental compatibility is documented as early-semver-style 0.x policy:
  breaking changes increment the minor version; patch releases remain
  compatible within that minor line.
- The exact backend now depends through `neutralScalameta` to `core`; existing
  released `core` and `frontend` API inventories remain separate and unchanged.

### Security

- No private reporting channel is currently offered or promised, and no
  production security review or response SLA is claimed for this experimental
  research release.
