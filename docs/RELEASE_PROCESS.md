# Experimental release process

This is a manual-first, fail-closed process. It documents local candidate
mechanics but does not authorize a remote release, tag, upload, or publication.
An ordinary branch push never runs artifact publication.

## Selected local candidate

- version: `0.2.0`;
- `io.github.dmytromitin:quasiquotes-scala3-core_3:0.2.0`, built once with
  Scala 3.3.8;
- `io.github.dmytromitin:quasiquotes-scala3-frontend_3.3.8:0.2.0`;
- `io.github.dmytromitin:quasiquotes-scala3-frontend_3.8.4:0.2.0`.

The Scala 3.9.0-RC1 frontend is a forward test probe and is excluded. The
aggregate root, `dottyInternal`, and example modules are also excluded.

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

## Local signed staging

The build pins sbt-pgp and sets `publishTo` to sbt's `localStaging` resolver.
`publishSigned` therefore writes to `target/sona-staging`; it does not upload.
Use a dedicated `GNUPGHOME`, confirm the candidate version, and run the exact
sequence:

```text
++3.3.8! ; core/publishSigned ; frontend/publishSigned
++3.8.4! ; frontend/publishSigned
```

The four developer properties must be passed to the sbt process. Core is
intentionally staged only on the 3.3.8 line so another compiler line cannot
overwrite `core_3` silently.

Validate the resulting repository with:

```text
python3 tools/release/check-release-repository.py PROJECT STAGING \
  --fingerprint PUBLIC_FINGERPRINT --source-identity SOURCE_ID \
  --json MANIFEST.json --markdown MANIFEST.md
```

The checker requires the exact three-coordinate set, POM/binary/sources/docs,
MD5 and SHA-1 checksums, detached signatures, local signature verification,
Apache-2.0 metadata and JAR contents, exact compiler dependencies, complete
developer metadata, and absence of unpublished modules.

## Candidate validation

Before any later release decision:

1. Freeze an exact clean source commit and supported JDK/sbt/Scala matrix.
2. Run the complete aggregate and example suites on Scala 3.3.8, 3.8.4, and
   the 3.9.0-RC1 forward probe.
3. Verify the compiler-free core boundary and module graph.
4. Generate and diff the public API inventory; stop on any delta.
5. Run first-use, documentation, content/workflow, license-distribution, and
   public/private-boundary checks.
6. Stage twice from independent roots/caches and compare all unsigned files.
   Signature bytes may differ because detached signatures contain creation
   metadata; unsigned differences require explanation and review.
7. Resolve and exercise the staged coordinates from fresh external projects.
   Verify that compiler mismatch and the excluded forward-probe coordinate do
   not resolve.

Every local bundle must be labeled as rehearsal evidence, not a release. A
remote release requires a separate authorization plus confirmed Central
account/namespace state, an owner-controlled publisher token outside Git,
approved public developer/signing identity, a real signing key and published
public fingerprint, a frozen candidate commit, and final green validation.
