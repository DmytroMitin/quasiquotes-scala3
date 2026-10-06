# N059 private selected-name TermTemplate completion handoff

## 1. Prompt hash and launch/final identities

- Prompt: `N059_private_selected_name_termtemplate_completion.prompt.md`
- SHA-256: `5dbe3f4a37aa5737624ee55626199612fdce2987c50937ff7dc8d6ee81862e61`
- Launch product SHA: `2e0d02b187f3f8dbfde6c2a6f4cd1045f48b1beb`
- Final local product HEAD / working-tree base: `2e0d02b187f3f8dbfde6c2a6f4cd1045f48b1beb`
- Final observed product `origin/main`: `00ba1afdd60382b02d8bd1830ed894a818a8010d`
- Launch control SHA: `2d998c1b35439856359580613516f0b642653c26`
- Final local control HEAD: `2d998c1b35439856359580613516f0b642653c26`
- Final observed control `origin/main`: `5e8cd22f671b5284a5232034d89e494f8f11ad2b`
- No product commit was created. N059 remains an unstaged working-tree handoff, as required by the standalone-push boundary.

## 2. Fresh roadmap/mailbox authority consumed

Fresh `origin/main` reads confirmed:

- `coordination/roadmaps/N.md`: N059 is ACTIVE and is the selected bounded private `TermShape.Select.name` completion task.
- `coordination/state/N.md`: `CURRENT_FOCUS = N059_PRIVATE_SELECTED_NAME_TERMTEMPLATE_COMPLETION`, with no N-local blocker while the current plain-name intersection is preserved.
- complete `coordination/messages/to-n/`: latest authority remains C-N-0015, the asynchronous lane-roadmap policy; no newer higher-priority READY C route arrived.
- accepted N058 conclusion B is the direct prerequisite and remains unchanged.

No coordination/state/roadmap file was edited and N060 was not allocated.

## 3. Exact file ledger

Modified production:

- `core/src/main/scala/quasiquotes/terms/TermConstructionError.scala`
- `core/src/main/scala/quasiquotes/terms/TermShapeTraversal.scala`
- `core/src/main/scala/quasiquotes/terms/TermTemplate.scala`
- `core/src/main/scala/quasiquotes/terms/TermTemplateSourceMetadata.scala`

Added tests:

- `core/src/test/scala/quasiquotes/terms/TermTemplateSelectedNameCompletionTest.scala`
- `neutral-scalameta/src/test/scala/quasiquotes/neutral/ScalametaSelectedNameTermTemplateCompletionTest.scala`

Added handoff:

- `reviews/n/N059_private_selected_name_termtemplate_completion_handoff/README.md`

No neutral production, public API, frontend, hybrid, dotty-internal, build, workflow, release, or programme-document file changed.

## 4. Metadata model and Select ordinal

The new private metadata is:

```scala
private[quasiquotes] final case class SelectedNameHoleOccurrence(
    name: String,
    selectOrdinal: Int
)
```

`TermShapeTraversal.selectEntries` defines a deterministic preorder over semantic `TermShape.Select` nodes. A Select records and increments its ordinal before its qualifier is traversed; all other recursive children follow the same stable structural order used by template completion. Every Select contributes exactly one ordinal, independently of its selected-name spelling.

Exactly zero or one selected-name occurrence is admitted per template. The registered ordinal must address the sole Select whose `name` equals the generated transport, and that transport must occur in exactly one String field in the tree.

## 5. Identifier/nonIdentifier ordinals were not abused

Selected names are String fields, not identifier-shaped Terms, so N059 does not encode them as scalar/repeated `TermHoleOccurrence` values or fake Identifier children. It also does not use the generic `nonIdentifierFields` position as an address because that stream mixes selected names with constructors, operators, interpolation prefixes/parts, and other Strings. `nonIdentifierFields` is used only as a fail-closed transport-collision count during creation.

## 6. Typed binding API

The private completion entry point is:

```scala
completeWithSelectedNames(
  scalarTermBindings: Map[String, ConstructedTerm],
  repeatedTermBindings: Map[String, Vector[ConstructedTerm]],
  selectedNameBindings: Map[String, SelectedMemberName],
  typeBindings: Map[String, TypeNormalForm]
)
```

Raw Strings cannot inhabit the selected-name map; a compile-time regression test proves the rejection. Existing `complete` and `completeWithRepeatedTerms` delegate with an empty selected-name map and retain their source and behavior compatibility.

## 7. Current-neutral lexical intersection guard

Completion admits only the current neutral intersection by calling the existing compiler-free `DefinitionName.plain` authority on `SelectedMemberName.decoded`. This enforces `[A-Za-z_][A-Za-z0-9_]*`, excluding `_` and Scala 3 keywords. Accepted evidence covers `ordinary`, `member2`, and `_privateLike`; rejected evidence covers `+`, `type`, `safe spaced name`, and `_`.

No neutral admission policy or `SelectedMemberName` grammar was changed. The use of the existing Core name authority avoids introducing another hand-written regex, though future changes to either neutral policy or `DefinitionName.plain` should keep their already-equal contracts synchronized.

## 8. Category and binding-set validation

Creation rejects:

- more than one selected-name logical name or occurrence;
- one selected transport appearing at two Select fields;
- missing/mismatched/out-of-range occurrence metadata;
- occurrence metadata outside `Select.name`;
- logical-name collision with scalar or repeated Term holes;
- generated-transport collision with scalar, repeated, or Type transports.

Completion independently rejects a null map, missing bindings, extra bindings, null values, and values outside the lexical intersection. The established template policy permits the same logical text in a Term-category hole and a Type hole, so N059 consistently permits selected-name/Type logical-name reuse while keeping their transport names disjoint.

## 9. Completion traversal integration

N059 extends the existing recursive `completeSubtree` engine rather than duplicating it. A per-completion local cursor consumes one Select ordinal at every Select node in preorder. Fixed names are preserved; the registered selected-name occurrence resolves through its typed binding and emits only the decoded String. A final invariant requires the cursor to equal the complete Select count. Ordinary `ConstructedTerm` validation still runs after the whole tree and sidecars are completed.

No marker, host wrapper, placeholder object, source-rank metadata, or transport spelling survives in the completed `TermShape`.

## 10. Scope/binder preservation proof

Selected-name substitution changes only the String field of an existing Select. The qualifier is recursively completed through the original engine, and BinderIds, bound references, sidecars, block ordering, and all existing scope checks remain untouched. A binder-bearing Lambda test proves the exact BinderId and bound receiver survive while only the selected name changes. No artificial binder-free restriction was added.

## 11. Equality/hash/render semantics

Semantic identity now represents a Select name as either `Fixed(name)` or `Hole(logicalName)`.

- fixed `ordinary` differs from a hole named `member`;
- holes `member` and `other` differ;
- the same logical hole at the same position compares and hashes equally across different generated transport Strings.

Debug rendering includes `selectedNameHoles=[logical@selectOrdinal]`; this remains private diagnostic output, not a serialization promise.

## 12. Source/LocatedTermTemplate boundary

N059 adds no frontend/source syntax and fabricates no source span. `LocatedTermTemplate.create` now fails closed with `InvalidLocatedTemplateMetadata("located scalar term templates do not support selected-name metadata")` whenever selected-name metadata is present.

## 13. Positive matrix

Covered and passing:

1. canonical receiver selected-name completion;
2. runtime-created `SelectedMemberName("member2")`;
3. fixed nested qualifier preserved exactly;
4. dedicated nested Select preorder with a nonzero selected ordinal;
5. binder-bearing Lambda scope neutrality;
6. scalar Term-hole coexistence;
7. repeated Term-hole coexistence and expansion;
8. Type sidecar coexistence, including the established same logical text policy;
9. transport-independent semantic equality/hash and final transport removal;
10. completed names author through neutral Scalameta, recursively retain `Position.None`, and reproject to the exact shape.

## 14. Negative/error matrix

Covered and passing:

- missing selected occurrence metadata;
- wrong/out-of-range Select ordinal;
- identifier-position metadata;
- duplicate same-name and distinct-name selected occurrences;
- repeated transport at two Select fields;
- selected/scalar and selected/repeated category collisions;
- missing and extra selected bindings;
- null selected binding map and null value;
- compile-time raw-String type mismatch;
- symbolic, keyword, spaced, and wildcard lexical-intersection rejection;
- invalid broader `SelectedMemberName` grammar rejected by its existing constructor;
- selected metadata rejected by `LocatedTermTemplate`;
- fixed selected names remain unchanged.

New errors are private and limited to invalid position, duplicate occurrence, category conflict, missing/extra/invalid binding, and lexical-intersection rejection.

## 15. Existing scalar/repeated/type completion unchanged

Old factories supply empty selected metadata, and old completion methods supply an empty selected binding map. The focused regression set passed all existing scalar, repeated, validation, equality, source-metadata, `ConstructedTerm`, and `SelectedMemberName` suites. Full Core passed 334/334 on every required compiler line and again in the final sequence.

## 16. Neutral lexical policy did not widen

`neutral-scalameta/src/main` has zero diff. Existing Select projection/authoring and N058 dynamic-selected-member characterization suites passed, including their existing rejection behavior. The new neutral suite exercises only completed names already inside the current intersection plus fixed-name compatibility. No neutral diagnostic category changed.

## 17. Public API and cross-lane delta

- Public signature delta: zero.
- Completed `TermShape` delta: zero.
- Neutral production delta: zero.
- Q/frontend/hybrid/U/dotty-internal production delta: zero.
- `SelectedMemberName` signature/grammar delta: zero.
- build/release/workflow delta: zero.

All new production surface is inside the existing `private[quasiquotes]` TermTemplate subsystem.

## 18. Focused tests

Command covered the new Core and neutral suites plus all prompt-named regression suites.

- Core focused total: 86 passed, 0 failed.
- Neutral focused total: 30 passed, 0 failed.

The first TDD run failed for the expected missing N059 types/API; the implemented run and final rerun passed.

## 19. Three-line matrix

Each line ran, in required order: `clean`, `core/test`, `neutralScalameta/test`, `core/verifyCoreBoundary`, `neutralScalameta/verifyNeutralScalametaBoundary`, `verifyModuleGraph`, and `verifyScalametaArtifactTopology`.

| Scala | Core | neutral | Core boundary | neutral boundary | module graph | artifact topology |
|---|---:|---:|---|---|---|---|
| 3.9.0 | 334/334 | 664/664 | PASS | PASS | PASS | PASS |
| 3.8.4 | 334/334 | 664/664 | PASS | PASS | PASS | PASS |
| 3.3.8 | 334/334 | 664/664 | PASS | PASS | PASS | PASS |

Boundary evidence on each line remained 73 Core sources / 1152 class-or-TASTy files / 3 compile and runtime entries, and 41 neutral sources / 302 class-or-TASTy files / 12 compile and runtime entries with Scalameta 4.17.3 and no compiler implementation, staging, or SemanticDB.

## 20. Final default-line six

The exact final sequence passed:

1. `neutralScalameta/test`: 664/664;
2. `core/test`: 334/334;
3. `neutralScalameta/verifyNeutralScalametaBoundary`: PASS;
4. `core/verifyCoreBoundary`: PASS;
5. `verifyModuleGraph`: PASS;
6. aggregate `test`: 1031/1031, with all module sub-totals green.

Expected negative-fixture compiler diagnostics appeared inside passing suites; the command exited 0.

## 21. Concurrency classification

The final fetch observed product `origin/main` advance from `2e0d02b` to `00ba1af` through `Add Scalameta dynamic tqq parity`. Its five changed files are confined to frontend/hybrid type-pattern production and Q053/Q056 tests. A direct path comparison returned no overlap with Core terms or neutral paths touched by N059.

Control `origin/main` advanced from `2d998c1` to `5e8cd22` through Q/U scheduling, acceptance, prompts, and handoffs. A direct diff of `coordination/roadmaps/N.md`, `coordination/state/N.md`, and `coordination/messages/to-n/` returned no change. Fresh reads still select N059, with no blocker or preemption.

The local dirty working tree was not rebased or merged during the handoff, avoiding any discard or unreviewed mutation. A future standalone publication must fetch-first, reconcile the path-disjoint main movement, rerun affected gates if the base changes, and never force-push.

## 22. Exact-SHA hosted CI state

`NOT_RUN` / pending. N059 has no product commit and no publication authority in this execution turn, so there is no exact N059 SHA on which hosted Scala 3.3.8 / 3.8.4 / 3.9.0 CI could run. Hosted exact-SHA success remains mandatory before controller acceptance.

## 23. Roadmap recommendation

Keep N059 review-pending. After a separately authorized standalone push, run/observe the mandatory exact-SHA hosted three-line CI and route the published handoff to the N controller. Accept N059 only if that CI passes and controller review confirms this private bounded scope. Do not allocate N060 from this execution.

## 24. SELF_ACCEPTANCE

`SELF_ACCEPTANCE = NO`

Local review result: no Critical, Important, or Minor issue found against the prompt. The normal review skill calls for a reviewer subagent, but active task rules prohibited subagent spawning; this limitation does not substitute for controller review.
