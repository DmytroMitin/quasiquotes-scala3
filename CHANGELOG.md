# Changelog

All notable user-visible changes will be recorded here. Entries are grouped
under `Added`, `Changed`, `Fixed`, `Deprecated`, `Removed`, and `Security` where
applicable. Dates and release links are added only when a release exists.

## Unreleased

### Added

- Compiler-free structural values, construction, matching, normal forms,
  source metadata, and a bounded public contextual-method description.
- Compiler-coupled source parsing, quoted lowering, diagnostics, term
  construction/matching, and typed type integration.
- Recursive `List` and `Option` plus binary `Either` type structures across
  source, matching, construction, TypeRepr, and scoped type evidence.
- Bounded structural standard-`s` interpolation support.
- External-package core and frontend examples and deterministic build-boundary
  checks.

### Changed

- Experimental compatibility is documented as early-semver-style 0.x policy:
  breaking changes increment the minor version; patch releases remain
  compatible within that minor line.

### Security

- No private reporting channel or released supported version exists yet.
