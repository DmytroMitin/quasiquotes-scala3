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

The current controller-accepted `0.3.0-SNAPSHOT` standard candidate has exactly
679 packaged rows / 661 symbol groups. Compared with released `0.2.0`, 609
signatures and 592 groups are unchanged, 70 signatures and 60 groups are added,
nine signatures are removed/replaced, and no symbol group is removed. The nine
replacements include the truthful statement-supertrait changes used by the
block/local-value model and the accepted `dqq` Scala/TASTy selector replacement;
later development also adds selected-Type identity, bounded definition,
selected-member, and composition surfaces.

The result remains
`BREAKING_API_SHAPE_DELTA_REQUIRES_NEW_0X_MINOR`. It does not mutate the
released 618-row file, and the 679-row candidate remains generated evidence
until an actual `0.3.0` release.

Against the preceding accepted development baseline, the scalable Definition
pattern-extractor change moved
677 rows / 659 groups to 679 rows / 661 groups: two groups and three signatures
were added, no group was removed, and one signature was replaced. The accepted
current inventory SHA-256 is
`a9753e1e737414e2f9e431723cbeb23a18add7eb81f65735bf61ebe84d6ee9b1`.
The additions are the scalable `DefinitionPatternExtractor` class and its
`unapply`; the replacement is the public Scala/TASTy `dqq` declaration with a
transparent-inline selector shape. The historical erased JVM descriptor is
preserved separately by a source-hidden bridge on Scala 3.3.8, 3.8.4, and
3.9.0. The current surface also adds semantic Tuple/Function single-parameter construction breadth
without changing these rows or groups.

The unpublished hybrid typed-Scalameta inventory remains 43 rows / 43 symbol
groups. Exact-two Definition parity replaces one Scala/TASTy selector signature
without adding or removing a symbol group. The historical erased JVM descriptor
is retained by a source-hidden bridge. This is no standard `core`/`frontend`
inventory delta.

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
