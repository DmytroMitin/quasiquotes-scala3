# Changelog

All notable user-visible changes will be recorded here. Entries are grouped
under `Added`, `Changed`, `Fixed`, `Deprecated`, `Removed`, and `Security` where
applicable. Dates and release links are added only when a release exists.

## Unreleased

### Changed
### Added

- A bounded public exact-version `ExistingClassUntypedRewrite` facade for one
  pre-Typer class transaction in the active 0.4.0-SNAPSHOT source tree.


- Active development continues as `0.4.0-SNAPSHOT` after the completed 0.3.0
  release. No 0.4.0 artifact, tag, or GitHub release exists.
- Public documentation now links the London Scala User Group presentation and
  states the post-0.3.0 Q/N/U-D/U-U/C development boundaries explicitly.

## 0.3.0 - 2026-09-07

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
- A binary-crossed `neutralScalameta` experiment using Scalameta
  4.17.3 for direct term/type/definition construction and matching, plus a
  bounded structural contextual-method projection into the existing validated
  IR.
- Exact-version frontend, typed-Scalameta frontend, and Dotty-internal
  coordinates for Scala 3.3.8, 3.8.4, and 3.9.0, completing the published
  eleven-coordinate topology.
- An exact-backend bridge for the admitted contextual method in
  both directions, including generated/no-position reverse matching without
  print/reparse.
- Public compiler-free Term, Type, and Definition semantic models and bounded
  projection/authoring and source-free or generated-origin lowering surfaces.
- The bounded runtime-sequence `tqr` Type-application construction overload.

### Changed

- `0.2.0` remains an immutable Maven Central release and public compatibility
  baseline. Version `0.3.0` is also published on Maven Central with an
  annotated `v0.3.0` tag and matching GitHub release.
- The build uses sbt 1.12.15 and sbt-pgp 2.3.1 for manual, local-only signed
  staging with fail-closed public developer metadata.
- Experimental compatibility is documented as early-semver-style 0.x policy:
  breaking changes increment the minor version; patch releases remain
  compatible within that minor line.
- The exact backend depends through `neutralScalameta` to `core`; standard and
  typed-Scalameta API inventories remain separate.

### Security

- No private reporting channel is currently offered or promised, and no
  production security review or response SLA is claimed for this experimental
  research release.
