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

The current `0.3.0-SNAPSHOT` candidate has 670 packaged rows. Compared with
released `0.2.0`, 611 signatures are unchanged, 59 are added, seven are
removed/replaced, 52 symbol groups are added, and no symbol group is removed.
The seven replacements include the truthful statement-supertrait changes used
by the block/local-value model; later development adds selected-Type identity,
bounded definition, selected-member, and composition surfaces relative to the
earlier development snapshots.

The result remains
`BREAKING_API_SHAPE_DELTA_REQUIRES_NEW_0X_MINOR`. It does not mutate the
released 618-row file, and the 670-row candidate remains generated evidence
until an actual `0.3.0` release.

The standard inventory deliberately excludes the unpublished, full-crossed
`dottyInternal` artifact, so it cannot describe that module's foreign-package
bridge surface. An explicit Scala-3.8.4 JVM artifact comparison against launch
commit `3ae93af527778ad1127f43ec7ffd2e325bae0008` records the new
`SelfAbstractTypeMemberPeerBridge` as an additive-only exact-version delta:
five class files and 40 public JVM members, or 45 normalized class/member rows.
There are no removed or replaced JVM rows in that bridge-specific comparison.
Those rows include Scala compiler-generated forwarders and case-class members;
the intended source contract remains the `lower` operation, categorized
`Failure`, and the three read-only `Lowered` fields. This is an experimental
compiler-internal API addition, not a stable-coordinate compatibility promise.

The separate [0.2-to-0.3 statement-ADT compatibility report](STATEMENT_ADT_0_2_TO_0_3_COMPATIBILITY.md)
proves bounded source, JVM-linkage, and TASTy consumer behavior. That evidence
is intentionally separate from this shape inventory.

## Explicit non-guarantees

The inventory cannot prove JVM binary compatibility, TASTy compatibility,
source overload or given resolution, semantic behavior, compiler-internal
compatibility, or runtime serialization compatibility. Binary/TASTy probes,
source-consumer compilation, and behavioral tests remain independent gates.
