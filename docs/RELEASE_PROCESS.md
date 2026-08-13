# Experimental release process

This is a manual-first, fail-closed process. It documents mechanics but does
not authorize a release.

## Required decisions and metadata

Apache License 2.0 is selected and recorded in the root `LICENSE`; intended
`core` and `frontend` POMs and JARs carry matching metadata. Before remote
publication, re-audit that distribution metadata and select an artifact host,
signing workflow where required, release version, the then-current security
policy, and explicit release authorization. Populate developer metadata only
from approved inputs. Source visibility remains a separate decision from
artifact publication.

## Candidate preparation

1. Freeze an exact clean source commit and supported JDK/sbt/Scala matrix.
2. Run the complete canonical build and compatibility lanes.
3. Generate and diff the public API inventory with the documented
   `tools/public-api/check-current.sh` current-minor gate. Stop on malformed
   input or removals; require explicit review for additions.
4. Build a sanitized public candidate and pass history, secret-pattern,
   documentation-link, and human content review.
5. Stage a non-final candidate version into a task-owned local Maven repository.
6. Publish only `core` and matching-line `frontend`; confirm the root and
   `dottyInternal` are absent.
7. Inspect POM, binary, source, and documentation artifacts and record hashes.
8. Run clean coordinate-only consumers from fresh caches.
9. Generate the complete local review bundle twice from independent fresh
   roots. Compare normalized manifests, API reports, coordinates, split/audit
   summaries, and candidate identity byte-for-byte.
10. Compare JARs, POMs, and generated checksums as raw release files. If bytes
   differ, identify the cause and compare semantic archive contents; never
   normalize the release files merely to claim reproducibility.

Every local bundle must be labeled as unsigned local review evidence, not a
release, not published, and without public-visibility authorization. It must
carry the canonical Apache-2.0 license in the POM and binary, source, and
documentation JARs.

## Authorized remote release

Only after the decisions above are complete may a separate task configure the
selected host and credentials, sign where required, publish a reviewed version,
verify remote coordinates from clean consumers, and separately create any tag
or release notes. Ordinary pushes must never publish artifacts. A release must
fail closed when version, license distribution, host, credentials, or signing
inputs are missing.

No dormant publication workflow is included yet: host, signing, version, and
release authorization remain unresolved, so a reviewed manual runbook is safer
than premature automation.
