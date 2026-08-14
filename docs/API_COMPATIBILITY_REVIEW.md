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

The accepted inventory contains 596 rows: 305 from `core` and 291 from
`frontend`. The exact-two definition first-use surface added 11 core rows to
the previous 585-row baseline:

- `TwoParameterMethodResultView`;
- `DefinitionConstruction.twoParameterMethod`;
- the result view's `kindCode`, `source`, `body`, `name`, ordered first/second
  parameter name/type accessors, and `resultType`.

The reviewed delta contained no removals, replacements, overload mutations, or
frontend additions. Regeneration against this accepted baseline must report
`NO_PUBLIC_API_DELTA`.

## Explicit non-guarantees

This tool is not a binary compatibility checker and cannot prove binary
compatibility, TASTy compatibility, source overload-resolution compatibility,
implicit/given search compatibility, semantic behavior compatibility,
compiler-internal compatibility, or runtime serialization compatibility.
