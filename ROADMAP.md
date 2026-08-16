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
- Preserve the bounded compiler-free one-ordinary-parameter definition core,
  core-only public identity-method first use, and unpublished exact backend
  without implying a general source adapter or placement policy.
- Maintain end-to-end recursive `List`/`Option` and binary `Either` support
  without turning fixed constructor admission into a general type resolver.
- Preserve a deterministic clean aggregate build and external-package examples.

## Before a public preview

- Reconfirm the Apache-2.0 provenance/attribution audit remains current.
- Keep security, support, contribution, and community-policy wording current
  with the experimental research status.
- Run independent source/history residual scanning and a human public-content
  audit for each visibility candidate.
- Keep the documented experimental early-semver and compiler-line policy
  aligned with the reviewed public API inventory.
- Reconsider whether a private security-reporting channel is warranted as the
  project and its support commitments evolve; none is currently promised.

## Before remote artifact publication

- Reconfirm the selected `0.2.0` version and Central Portal publication path.
- Replace synthetic rehearsal identity with explicitly approved public
  developer and signing identity inputs.
- Complete real-key signing, provenance, POM, source/Javadoc, and
  reproducibility checks.
- Validate clean coordinate-only consumers on every promised Scala/JDK lane.
- Publish only `core_3`, `frontend_3.3.8`, and `frontend_3.8.4`; keep the
  forward-probe frontend, aggregate root, examples, and `dottyInternal`
  unpublished.

Remote release and public visibility are separate decisions. If either remains
unapproved, development continues through bounded language, usability,
compatibility, and backend improvements.
