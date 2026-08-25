# Public API shape compatibility review

The machine baseline is generated from packaged `core` and `frontend`
Scaladoc search metadata. It normalizes every visible entry to:

```text
module  owner  kind  name  signature
```

The released snapshot is [docs/api-baselines/0.2.0.tsv](api-baselines/0.2.0.tsv).
It has 618 data rows: 305 from `core` and 313 from `frontend`. The versioned
name is deliberate: this file is immutable evidence for released `0.2.0`, not
a timeless description of the current development tree.

Scaladoc is both the human API documentation and the metadata input. The TSV
is a deterministic source/API-shape comparison artifact; it is not a
replacement for Scaladoc and is not itself a compatibility promise.

## Recommended command

After `sbt -batch core/packageDoc frontend/packageDoc`, run:

```text
tools/public-api/check-current.sh docs/api-baselines/0.2.0.tsv CORE_DOCS_JAR FRONTEND_DOCS_JAR OUTPUT_DIR current-minor
```

The diff groups rows by `(module, owner, kind, name)` and compares exact
signature sets. Exit `0` means no exact inventory delta. Exit `2` means
additions only and requires explicit review. Exit `3` means at least one
removal and requires a new experimental 0.x minor. Exit `4` means malformed
or unsupported input and fails closed. Owner, module, or kind moves are
reported conservatively as removal plus addition.

## Released baseline and current development candidate

The current `0.3.0-SNAPSHOT` candidate has 655 packaged rows. Compared with
released `0.2.0`, 614 signatures are unchanged, 41 are added, four are
removed/replaced, and no symbol group is removed. The four replacements arise
from the truthful statement supertraits used by the new block/local-value
model. Canonical global selected-Type identity and its explicit environment
add 21 rows relative to the earlier 634-row development shape.

The result remains
`BREAKING_API_SHAPE_DELTA_REQUIRES_NEW_0X_MINOR`. It does not mutate the
released 618-row file, and the 655-row candidate remains generated evidence
until an actual `0.3.0` release.

The separate [0.2-to-0.3 statement-ADT compatibility report](STATEMENT_ADT_0_2_TO_0_3_COMPATIBILITY.md)
proves bounded source, JVM-linkage, and TASTy consumer behavior. That evidence
is intentionally separate from this shape inventory.

## Explicit non-guarantees

The inventory cannot prove JVM binary compatibility, TASTy compatibility,
source overload or given resolution, semantic behavior, compiler-internal
compatibility, or runtime serialization compatibility. Binary/TASTy probes,
source-consumer compilation, and behavioral tests remain independent gates.
