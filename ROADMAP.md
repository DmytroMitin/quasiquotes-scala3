# Product roadmap

This experimental roadmap describes product capability rather than internal
delivery chronology.

## Current

- Keep `core` compiler-free and independently consumable.
- Keep `frontend` compiler-version-coupled and test source parsing, matching,
  construction, diagnostics, and quoted lowering together.
- Keep `dottyInternal` source visible but its artifact unpublished.
- Preserve Apache-2.0 POM and JAR metadata for intended `core` and `frontend`
  distributions.
- Expand structural term and type support through narrow, test-backed slices.
- Preserve the accepted compiler-free one-ordinary-parameter definition core
  while its public first-use/source metadata and exact backend remain separate
  later gates.
- Maintain end-to-end recursive `List`/`Option` and binary `Either` support
  without turning fixed constructor admission into a general type resolver.
- Preserve a deterministic clean aggregate build and external-package examples.

## Before a public preview

- Reconfirm the Apache-2.0 provenance/attribution audit remains current.
- Complete security, support, contribution, and community-policy review.
- Run independent source/history secret scanning and a human public-content
  audit.
- Freeze a versioning and compatibility promise that matches the experimental
  compiler-internal surface.
- Establish a real private security-reporting channel.

## Before remote artifact publication

- Select a non-SNAPSHOT version and release repository.
- Complete signing, provenance, POM, source/Javadoc, and reproducibility checks.
- Validate clean coordinate-only consumers on every promised Scala/JDK lane.
- Publish only `core` and `frontend`; keep the aggregate root and
  `dottyInternal` unpublished.

Remote release and public visibility are separate decisions. If either remains
unapproved, development continues through bounded language, usability,
compatibility, and backend improvements.
