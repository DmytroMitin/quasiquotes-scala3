# Statement ADT compatibility: 0.2.0 to 0.3 development

The current source tree remains the unpublished `0.3.0-SNAPSHOT` development
line. The compatibility baseline is the immutable Maven Central `0.2.0`
release, not a reconstructed checkout. The statement-ADT change remains an
intentional new-minor change under `early-semver`; this qualification does not
authorize a release or remote snapshot publication.

## Released and development artifacts

The released artifacts used by the lab were downloaded from Maven Central:

| Coordinate | Scala meaning | Binary JAR SHA-256 | TASTy entries |
| --- | --- | --- | ---: |
| `com.github.dmytromitin:quasiquotes-scala3-core_3:0.2.0` | compiler-free, Scala 3 binary-crossed; POM uses Scala 3.3.8 | `36c9f18c358284dd263c5ae44ccc0d196387964f384b8b88222e5d56bf599fdd` | 91 |
| `com.github.dmytromitin:quasiquotes-scala3-frontend_3.3.8:0.2.0` | compiler-coupled, exact/full-crossed 3.3.8 | `8a35abb88f1a705b38e474d0d7e7d2d2c98309445587a675a8b77d1ea56c0700` | 90 |
| `com.github.dmytromitin:quasiquotes-scala3-frontend_3.8.4:0.2.0` | compiler-coupled, exact/full-crossed 3.8.4 | `00a2b658506ed3ed44a45b9472863c91374a956041d2c8baac02b5f5d2b6ccee` | 90 |

Their Central SHA-1 sidecars were verified byte-for-byte. The development tree
at source commit `106b192f3890440190feae81b47674f6458d1a36` was published only
to a collision-safe disposable local Maven repository. Resolver probes
selected Maven Central for `0.2.0` and the local repository for the development
build. No released version was shadowed and nothing was published remotely.

## Public API and source compatibility

The authoritative packaged-Scaladoc diff remains:

```text
released baseline rows = 618
current candidate rows = 634
unchanged rows = 614
added signatures = 20
removed/replaced signatures = 4
removed symbol groups = 0
classification = BREAKING_API_SHAPE_DELTA_REQUIRES_NEW_0X_MINOR
```

The four replacements are the public roots `TermShape`, `TermPattern`,
`CanonicalTerm`, and `TargetTermView` gaining their corresponding statement
supertrait. The new `Block` cases use statement-list prefixes.

There is an important released-baseline fact: Maven Central `0.2.0` contains
neither `TermShape.Block` nor `TermPattern.Block`. Direct block construction,
block destructuring, and a block-prefix assumption therefore cannot be valid
`0.2.0` downstream source examples. They are new development APIs, not removed
released APIs. Representative source using the four existing roots and
ordinary leaf cases compiled unchanged against real `0.2.0` and current `0.3`
on Scala 3.3.8 and 3.8.4. Current direct construction and destructuring of both
new block cases also compiled on both lines.

A source assumption that a new block's prefix is `List[TermShape]` does not
compile: the truthful type is `List[BlockStatement]` (and analogously
`List[BlockPatternStatement]`). Expression-only consumers should explicitly
collect or pattern-match the statement entries:

```scala
val expressionPrefixes: List[TermShape] =
  block.statements.collect { case term: TermShape => term }
```

This is a migration for unpublished pre-statement-ADT development usage, not a
confirmed source break in the released `0.2.0` block surface, because that
surface did not exist.

```text
SOURCE_COMPATIBILITY = SOURCE_COMPATIBLE_FOR_TESTED_RELEASED_0_2_SURFACE
NEW_0_3_BLOCK_PREFIX_SOURCE = STATEMENT_AWARE_MIGRATION_REQUIRED
```

## JVM binary compatibility

`javap -p -s -v` shows that all methods and JVM descriptors on the four
existing public roots are unchanged. Their only classfile-header change is one
new implemented interface: the corresponding statement trait. Representative
existing leaf case classes retain identical constructor, accessor, copy,
product, and companion/static descriptor surfaces on both exact frontend
lines and the binary-crossed core line.

The new `Block` cases have no released classfile counterpart. Their generic
signatures name statement-list element types, while erasure uses
`scala.collection.immutable.List`; no old `Block` linkage claim is meaningful.

A downstream application compiled against real `0.2.0` was run without
recompilation after substituting the corresponding current core/frontend JARs.
It constructed and used `TermShape`, `TermPattern`, `CanonicalTerm`, and
`TargetTermView` leaves successfully on Scala 3.3.8 and 3.8.4.

```text
JVM_BINARY_COMPATIBILITY = JVM_BINARY_COMPATIBLE_FOR_TESTED_SURFACE
```

This is a bounded classfile and linkage result, not a whole-library binary
compatibility guarantee.

## Scala 3 TASTy compatibility

For each supported compiler line, stage one compiled a downstream artifact
against real `0.2.0`. Its public TASTy exposed all four affected root types and
included inline methods, making TASTy reading necessary. Stage two compiled
against that unchanged stage-one artifact while substituting the current
`0.3` evidence JARs. Both stages and the inline expansion succeeded on Scala
3.3.8 and 3.8.4. The same stage-one source also recompiled against current
artifacts. Unaffected controls were included in the same lab.

There is no old downstream TASTy that can expose the new statement or block
types because those types are absent from `0.2.0`. Compatibility for those new
types begins with the `0.3` line.

```text
TASTY_COMPATIBILITY_3_3_8 = TASTY_COMPATIBLE_FOR_TESTED_SURFACE
TASTY_COMPATIBILITY_3_8_4 = TASTY_COMPATIBLE_FOR_TESTED_SURFACE
```

## Regression and governance result

The current tree passes the complete aggregate, focused statement/P1/Lambda1,
Scalameta Term and Type, public example, compiler-free boundary, neutral
boundary, module-graph, Scalameta topology, and affected packaging checks on
Scala 3.3.8 and 3.8.4. Scala 3.9.0-RC1 remains an experimental forward probe,
not a promoted support promise. The public API, first-use, documentation, and
release-configuration tools remain separate checks.

`docs/api-baselines/0.2.0.tsv` remains the immutable released 618-row
`0.2.0` baseline. The 634-row development inventory remains generated
candidate evidence until an actual `0.3.0` release; a redundant checked-in
candidate snapshot was not added.

The result is a bounded compatibility qualification for an intentional new
experimental minor. Development remains `0.3.0-SNAPSHOT`; released `0.2.0`
and its baseline remain immutable. Ordinary Term and Type quasiquotes continue
to use current-Dotty. This report is evidence, not release authorization.
