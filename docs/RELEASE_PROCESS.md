# Experimental release process

This is a manual-first, fail-closed process. The `0.2.0` set below is already
immutable on Maven Central. The `0.3.0` set is a candidate topology only: the
source tree remains `0.3.0-SNAPSHOT`, and this document does not authorize a
remote upload, tag, or GitHub release. An ordinary branch push never publishes
artifacts.

## Released immutable 0.2.0 artifact set

- `com.github.dmytromitin:quasiquotes-scala3-core_3:0.2.0`, built once with
  Scala 3.3.8;
- `com.github.dmytromitin:quasiquotes-scala3-frontend_3.3.8:0.2.0`;
- `com.github.dmytromitin:quasiquotes-scala3-frontend_3.8.4:0.2.0`.

Scala 3.9.0-RC1 and the aggregate, Scalameta, exact-backend, and example
modules were not part of `0.2.0`. The release checker retains this exact
historical contract as release set `0.2.0`; it must not be reconstructed by
scanning arbitrary staged content.

## Candidate 0.3.0 artifact set

The separately authorized future release candidate contains exactly eleven
coordinates:

```text
com.github.dmytromitin:quasiquotes-scala3-core_3:0.3.0
com.github.dmytromitin:quasiquotes-scala3-neutral-scalameta_3:0.3.0
com.github.dmytromitin:quasiquotes-scala3-frontend_3.3.8:0.3.0
com.github.dmytromitin:quasiquotes-scala3-scalameta-frontend_3.3.8:0.3.0
com.github.dmytromitin:quasiquotes-scala3-dotty-internal_3.3.8:0.3.0
com.github.dmytromitin:quasiquotes-scala3-frontend_3.8.4:0.3.0
com.github.dmytromitin:quasiquotes-scala3-scalameta-frontend_3.8.4:0.3.0
com.github.dmytromitin:quasiquotes-scala3-dotty-internal_3.8.4:0.3.0
com.github.dmytromitin:quasiquotes-scala3-frontend_3.9.0:0.3.0
com.github.dmytromitin:quasiquotes-scala3-scalameta-frontend_3.9.0:0.3.0
com.github.dmytromitin:quasiquotes-scala3-dotty-internal_3.9.0:0.3.0
```

`core_3` and `neutral-scalameta_3` are binary-crossed and staged once from the
Scala 3.3.8 session, the oldest supported line; those exact artifact bytes must
be consumed successfully from Scala 3.3.8, 3.8.4, and 3.9.0. The other nine
coordinates are full-crossed and must be built with their exact compiler line.
The aggregate and example modules remain non-published in every mode.

Maven availability does not make the experimental modules stable. In
particular, `dotty-internal` is exact-compiler-version coupled, follows 0.x
compatibility, exposes only its documented foreign-package bridges as intended
consumer seams, and makes no stability promise for package-private/internal
raw-tree machinery. It is not a generic public `untpd`/`tpd` toolkit.

## Normal project publishability and fail-closed release controls

`core`, `frontend`, `neutralScalameta`, `hybridScalametaFrontend`, and
`dottyInternal` are normally publishable production projects. They need no
special JVM property for packaging or task-owned local staging. The aggregate
root and example projects remain publication-skipped.

Normal project publishability is separate from a remote publication
transaction. `verifyScalametaArtifactTopology` validates the production/root/
example split as well as coordinate crossing, POM closure, packaging, licenses,
typed-API confinement, and checkout-contamination guards. Version selection,
the exact coordinate allowlist, developer identity, signing, terminal hosted-CI
review, credentials, and explicit owner authorization remain independent
fail-closed release controls.

Both `publish` and `publishSigned` for `core` and `neutralScalameta` also fail
closed unless the active Scala version is exactly 3.3.8. This prevents a newer
default or ad hoc session from replacing the single `_3` coordinates with
newer TASTy while older advertised consumers remain supported.

## Required public identity

Signed staging fails closed unless all public developer fields are supplied
explicitly as JVM system properties:

```text
quasiquotes.release.developer.id
quasiquotes.release.developer.name
quasiquotes.release.developer.email
quasiquotes.release.developer.url
```

These values are intentionally absent from the repository until approved.
Git author data and machine-local configuration are not substitutes. Synthetic
`.invalid` values may be used only with a disposable local key and staging root
for a structural rehearsal; they must never be treated as release identity.

## Local signed 0.3.0 rehearsal

The committed development version stays `0.3.0-SNAPSHOT`. Each rehearsal sbt
session applies the disposable session setting
`set ThisBuild / version := "0.3.0"`; because it is a `ThisBuild` setting, all
project POM identities and inter-project dependency versions change together.
Nothing is written back to `build.sbt`.

The build pins sbt-pgp and directs `publishSigned` to the Maven-style local
repository under `target/sona-staging`. Start from an empty staging directory,
use a disposable `GNUPGHOME`, pass synthetic developer properties, and execute
the dependency-safe sessions exactly once per coordinate:

```text
sbt <developer-properties> -batch \
  '++3.3.8!' \
  'set ThisBuild / version := "0.3.0"' \
  'core/publishSigned' \
  'neutralScalameta/publishSigned' \
  'frontend/publishSigned' \
  'hybridScalametaFrontend/publishSigned' \
  'dottyInternal/publishSigned'

sbt <developer-properties> -batch \
  '++3.8.4!' \
  'set ThisBuild / version := "0.3.0"' \
  'frontend/publishSigned' \
  'hybridScalametaFrontend/publishSigned' \
  'dottyInternal/publishSigned'

sbt <developer-properties> -batch \
  '++3.9.0!' \
  'set ThisBuild / version := "0.3.0"' \
  'frontend/publishSigned' \
  'hybridScalametaFrontend/publishSigned' \
  'dottyInternal/publishSigned'
```

Do not stage `core` or `neutralScalameta` in the second or third session: their
binary-cross coordinates would be duplicates, and rebuilding the same `_3`
coordinate with newer TASTy would violate the compatibility baseline.

Validate the candidate result with the explicit checker profile:

```text
python3 tools/release/check-release-repository.py PROJECT STAGING \
  --release-set 0.3.0-candidate \
  --fingerprint SYNTHETIC_PUBLIC_FINGERPRINT \
  --source-identity SOURCE_ID \
  --json MANIFEST.json --markdown MANIFEST.md
```

For an immutable `0.2.0` repository use `--release-set 0.2.0`. The checker
requires the profile's exact coordinate set and role-specific dependency
closure; POM, binary, sources, and javadoc artifacts; detached signatures;
MD5/SHA-1 sidecars for deployables and signatures; POM/developer/license/SCM
metadata; matching JAR licenses; and absence of local paths, unexpected
versions, root/examples, and RC coordinates.

## Candidate validation and external consumers

Before any remote-release decision:

1. Freeze an exact clean source commit and supported JDK/sbt/Scala matrix.
2. Run full aggregate/module tests on required Scala 3.3.8, 3.8.4, and final
   3.9.0 lanes.
3. Verify Core/neutral boundaries, module graph, effective publication settings, all
   binary/source/doc packages, public examples, API inventories, released
   `0.2.0` comparison, docs/content/workflow hygiene, and first-use snippets.
4. Resolve the eleven staged coordinates from fresh external projects without
   `ProjectRef`, checkout sources, or checkout class directories. Exercise the
   compiler-free Core/neutral route and both exact typed Scalameta and
   documented foreign-package bridge lines. Prove exact compiler mismatch
   fails rather than falling back.
5. Inspect the staged repository and manifest for exact coordinates,
   dependency closure, signatures/checksums, licenses, and path leakage.

Every local bundle is rehearsal evidence, not a release. Remote publication
requires a new explicit authorization plus confirmed Central account and
namespace state, an owner-controlled publisher token outside Git, approved
public developer/signing identity, a real signing key and published
fingerprint, an exact candidate commit with terminal hosted-CI review, and a
separately approved tag/release transaction.
