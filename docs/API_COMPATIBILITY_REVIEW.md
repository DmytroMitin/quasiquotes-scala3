# Public API shape compatibility review

The public API inventory is generated from packaged `core` and `frontend`
Scaladoc search data. It records public classes, definitions, enums, enum
cases, extensions, givens, objects, traits, types, values, and variables as:

```text
module  owner  kind  name  signature
```

The exact diff groups rows by `(module, owner, kind, name)` and compares the
exact signature set within each group. This preserves overload facts without
assuming the group key is globally unique. Owner, module, or kind moves are
reported conservatively as removal plus addition. The tool performs no fuzzy
move or rename inference.

## Recommended command

After `sbt -batch core/packageDoc frontend/packageDoc`, run:

```text
tools/public-api/check-current.sh docs/PUBLIC_API_BASELINE.tsv CORE_DOCS_JAR FRONTEND_DOCS_JAR OUTPUT_DIR current-minor
```

Exit `0` means no exact inventory delta. Exit `2` means additions only and
requires explicit review; a new overload, given, or extension is not
automatically patch-safe. Exit `3` means at least one removal and requires a
new experimental 0.x minor. Exit `4` means malformed or unsupported input and
fails closed. `report` mode returns zero for valid additive/breaking reports
but preserves the same classification and artifacts.

The machine report is deterministically sorted and the human summary maps the
exact shape result to the project's experimental `early-semver` policy. The
baseline remains a review artifact, not a stability promise.

## Current accepted shape

The accepted inventory contains 598 rows: 305 from `core` and 293 from
`frontend`. Relative to the frozen 596-row inventory, the bounded term-pattern
extractor review recorded exactly:

- removal of `QuasiPattern.qq: Nothing`;
- addition of `QuasiPattern.qq(using q: Quotes):
  TermPatternExtractor[q.reflect.Term]`;
- addition of public `TermPatternExtractor[T]` and its
  `unapplySeq(value: T): Option[Seq[T]]` protocol.

This is one deliberate signature replacement plus two new API groups: three
added signatures and one removed signature, for a net two-row increase. The
old member was intentionally unusable, but source references, compiled code,
and TASTy that name its old signature are not compatible. The exact-shape gate
therefore required a new experimental 0.x minor, and the source line is now
`0.2.0-SNAPSHOT`. Regeneration against this accepted baseline must report
`NO_PUBLIC_API_DELTA`.

## Explicit non-guarantees

This tool is not a binary compatibility checker and cannot prove binary
compatibility, TASTy compatibility, source overload-resolution compatibility,
implicit/given search compatibility, semantic behavior compatibility,
compiler-internal compatibility, or runtime serialization compatibility.
