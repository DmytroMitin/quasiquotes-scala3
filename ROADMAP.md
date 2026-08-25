# Product roadmap

This experimental roadmap describes product capability rather than internal
delivery chronology.

## Current

- Preserve one semantic architecture: source frontends project into the
  project-owned compiler-free model in `core`, followed by backend-specific
  lowering or reflected matching. Keep current-Dotty as the released/default
  reference route and Scalameta as explicit unpublished opt-in routes.
- Keep `core` compiler-free and independently consumable.
- Keep `frontend` compiler-version-coupled and test source parsing, matching,
  construction, diagnostics, and quoted lowering together.
- Keep the binary-crossed `neutralScalameta` experiment unpublished and
  isolated from compiler implementation, staging, and SemanticDB dependencies.
  Use direct Scalameta authoring until a genuinely reusable façade can delegate
  upstream macros without forwarding or duplicating them.
- Keep the compiler-coupled `hybridScalametaFrontend` experiment unpublished
  and side by side with the current term engine. Expand its admitted `qr`/`qq`
  slice only through differential tests, exact-compiler validation, original
  reflected-hole/capture identity, and compiler-line dialect selection; do not
  switch the public default without a separate compatibility decision.
- Keep `dottyInternal` source visible but its artifact unpublished. Retain only
  the narrow exact-version `ContextualMethodPeerBridge` for tightly coupled
  foreign-package peers; do not grow it into a generic raw-tree API.
- Preserve Apache-2.0 POM and JAR metadata for intended `core` and `frontend`
  distributions.
- Expand structural term and type support through narrow, test-backed slices.
- Preserve the bounded compiler-free one-ordinary-parameter definition core,
  core-only public identity-method first use, and unpublished exact backend
  without implying a general source adapter or placement policy.
- Maintain end-to-end recursive `List`/`Option` and binary `Either` support
  without turning fixed constructor admission into a general type resolver.
- Preserve a deterministic clean aggregate build and external-package examples.
- Preserve the bounded structural Scalameta-to-validated-IR projection and the
  exact backend-only reverse projection without print/reparse or fabricated
  source provenance.
- Measure whether public Scalameta parsing materially reduces grammar
  maintenance while current-Dotty remains the exact compiler oracle and only
  Scalameta parse failure may use the current parser as fallback.

## North-star source-like generation

The durable [north-star checkpoint document](docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md)
keeps the manual reflection baseline, conceptual future source-like shape,
missing capabilities, and completion criterion separate for every example.
All conceptual spellings remain non-current notation rather than selected
syntax.

| Checkpoint | Current status | Enabling gap |
| --- | --- | --- |
| N1 generic subclass with override | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | classes, overrides, dynamic parent Type, constructor/body lowering, backend symbol planning |
| N2 runtime-length dynamic Type application | `CURRENT_MANUAL_BASELINE_PROVED`, `PARTIALLY_COVERED_BY_CURRENT_QUASIQUOTES`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | constructor-position Type hole and sequence Type splice |
| N3 generated Type refinement members | `CURRENT_MANUAL_BASELINE_PROVED`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | refinement/type-member model and sequence definition splice |
| N4 anonymous implementation with calculated definitions | `PARTIALLY_COVERED_BY_CURRENT_QUASIQUOTES`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | anonymous-class bodies, broader definitions, sequence splices, owner planning |
| N5 dynamic `new T(..args)` for an existing type | `CURRENT_MANUAL_BASELINE_PROVED`, `PARTIALLY_COVERED_BY_CURRENT_QUASIQUOTES`, `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` | constructor-position Type hole, sequence Term splice, constructor/argument policy |

None of N1-N5 is `CHECKPOINT_COMPLETE`.

The leading immediate Term candidate remains a bounded dynamic member/name-hole
design checkpoint: a macro-computed member name currently requires manual
`Select`, `Select.unique`, or related APIs. The leading Type candidate remains
stable-term selected-Type prefix identity. The broader enabling themes are
dynamic type-position holes, sequence splices, and broader definition/class
support; this documentation does not impose a final implementation order.

Symbols are compiler semantic entities, not source syntax. No public symbol
quasiquote family is currently planned. The neutral core remains symbol-free;
typed owned-definition symbol synthesis belongs to the typed backend, while an
untyped pre-typer backend must not fabricate typed symbols. A future advanced
owner/definition-plan handle needs concrete consumer evidence.

## Ongoing public-project hygiene

- Reconfirm the Apache-2.0 provenance/attribution audit remains current.
- Keep security, support, contribution, and community-policy wording current
  with the experimental research status.
- Run independent source/history residual scanning and a human public-content
  audit for each visibility candidate.
- Keep the documented experimental early-semver and compiler-line policy
  aligned with the reviewed public API inventory.
- Reconsider whether a private security-reporting channel is warranted as the
  project and its support commitments evolve; none is currently promised.

## Before a later artifact release

- Select the next version from reviewed API and compatibility evidence and
  reconfirm the Central Portal publication path.
- Use explicitly approved public developer and signing identity inputs.
- Complete real-key signing, provenance, POM, source/Javadoc, and
  reproducibility checks.
- Validate clean coordinate-only consumers on every promised Scala/JDK lane.
- Publish only separately approved coordinates; keep the
  forward-probe frontend, aggregate root, examples, `neutralScalameta`,
  `hybridScalametaFrontend`, and `dottyInternal` unpublished.

Later releases remain separate decisions. Development continues through
bounded language, usability, compatibility, and backend improvements without
assuming publication.
