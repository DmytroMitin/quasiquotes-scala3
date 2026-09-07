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

The current `0.3.0-SNAPSHOT` standard candidate has 794 packaged rows / 775
symbol groups on the modern compiler lines. Compared with released `0.2.0`,
609 signatures and 592 groups are unchanged, 185 signatures and 174 groups
are added, nine signatures are removed/replaced, and no symbol group is removed.
The result remains `BREAKING_API_SHAPE_DELTA_REQUIRES_NEW_0X_MINOR`.
The released 618-row baseline remains immutable; the candidate inventory is
generated development evidence until an actual `0.3.0` release.

The standard typed runtime-sequence `tqr` overload adds one logical row to
the previous 793-row / 775-group inventory: all 793 prior rows are unchanged,
with no replacement, removal or symbol-group addition. Its existing umbrella
export produces a JVM forwarder without another logical Scaladoc row. This
additive change preserves the established new-minor classification against
`0.2.0`; it does not erase the nine earlier replacements.

The semantic Term generated-origin facade changes no Core/frontend production
source or standard inventory declaration. Scala 3.3.8's Scaladoc spells five
existing derived-given/context-function signatures differently from the modern
compiler inventory, while retaining 794 rows / 775 groups. Compare exact
compiler lines before interpreting a signature-rendering difference as a
product API change.

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

The implemented [semantic Term generated-origin facade](SEMANTIC_TERM_GENERATED_ORIGIN_LOWERING.md)
adds the selected `TermGeneratedOriginLowering`, `Failure` and `Lowered` source
contract to that same exact-version Dotty artifact. Its five class files and one TASTy entry include generated case-class/forwarder
members whose count differs by compiler line; exact inventories are compared
within each line. The shared checked seam and source-admission helper remain
Scala package-private; their JVM encodings are separate implementation inventory.
The source-free Term facade keeps its public signature. Historical Scalameta Term
bridge entries remain byte-identical; unchanged Definition facade TASTy may differ
in serialization after clean rebuilding while decompiled declarations and JVM
behavior remain identical. External artifact consumers check the selected public
surface and reject direct Lowered construction and private helper access.

The separate [0.2-to-0.3 statement-ADT compatibility report](STATEMENT_ADT_0_2_TO_0_3_COMPATIBILITY.md)
proves bounded source, JVM-linkage, and TASTy consumer behavior. That evidence
is intentionally separate from this shape inventory.

## Explicit non-guarantees

The inventory cannot prove JVM binary compatibility, TASTy compatibility,
source overload or given resolution, semantic behavior, compiler-internal
compatibility, or runtime serialization compatibility. Binary/TASTy probes,
source-consumer compilation, and behavioral tests remain independent gates.
