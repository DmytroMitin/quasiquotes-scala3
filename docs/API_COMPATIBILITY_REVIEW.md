# Public API shape compatibility review

The public API inventory is generated from packaged `core` and `frontend`
Scaladoc search data. It records public classes, definitions, enums, enum
cases, extensions, givens, objects, traits, types, values, and variables as:

```text
module  owner  kind  name  signature
```

The exact diff groups rows by `(module, owner, kind, name)` and compares the
exact signature set within each group. This preserves overload facts without
assuming the group key is globally unique. Owner, module, or kind moves are
reported conservatively as removal plus addition. The tool performs no fuzzy
move or rename inference.

## Recommended command

After `sbt -batch core/packageDoc frontend/packageDoc`, run:

```text
tools/public-api/check-current.sh docs/PUBLIC_API_BASELINE.tsv CORE_DOCS_JAR FRONTEND_DOCS_JAR OUTPUT_DIR current-minor
```

Exit `0` means no exact inventory delta. Exit `2` means additions only and
requires explicit review; a new overload, given, or extension is not
automatically patch-safe. Exit `3` means at least one removal and requires a
new experimental 0.x minor. Exit `4` means malformed or unsupported input and
fails closed. `report` mode returns zero for valid additive/breaking reports
but preserves the same classification and artifacts.

The machine report is deterministically sorted and the human summary maps the
exact shape result to the project's experimental `early-semver` policy. The
baseline remains a review artifact, not a stability promise.

## Phase-115 source candidate

The binder-free P1 block tranche generates 622 rows: 307 from `core` and 315
from `frontend`. Against the immutable released 618-row baseline, the exact
report records four additions, no removals, no overload/signature additions,
and no owner or kind moves:

- `quasiquotes.parser.TermShape.Block`;
- `quasiquotes.matching.TermPattern.Block`;
- `quasiquotes.matching.CanonicalTerm.Block`;
- `quasiquotes.matching.TargetTermView.Block`.

This is `ADDITIVE_API_SHAPE_DELTA_REVIEW_REQUIRED` and is deliberate future
source growth, not an automatic patch-safe or binary/TASTy compatibility
claim. The unpublished Scalameta opt-in implementation adds behavior only and
no separate public inventory row. Released `0.2.0` artifacts and the accepted
baseline below remain unchanged.

Phase 116 re-runs the same inventory after adding the P2 local-val behavior.
The truthful definition-statement boundary produces exactly 634 rows. Against
the immutable 618-row baseline, 614 rows are unchanged, 20 signatures are
added, and four signatures are removed. The removals are the old bare
supertrait signatures for `TermShape`, `TermPattern`, `CanonicalTerm`, and
`TargetTermView`; each is replaced by a signature extending its corresponding
new statement supertrait. The additions comprise those four replacement
signatures, four changed `Block` signatures over statement lists, and the four
statement traits/companions with their derived `CanEqual` rows. There are no
removed symbol groups.

This is the deliberate `BREAKING_API_SHAPE_DELTA_REQUIRES_NEW_0X_MINOR`
classification required by a local definition being a statement rather than a
term expression. It is not accidental exposure and does not mutate released
`0.2.0`; any future artifact containing this candidate requires a new
experimental 0.x minor and fresh binary/TASTy qualification.

## Current accepted shape

The accepted inventory contains 618 rows: 305 from `core` and 313 from
`frontend`. Relative to the immediately prior 616-row inventory, the bounded
body-only definition extractor review recorded exactly two additions and no
removals:

- `DefinitionPattern.dqq(using q: Quotes)` on `StringContext`;
- `SingleParameterDefinitionPattern.unapply(using q: Quotes)` returning the
  original RHS `q.reflect.Term`.

The generated review found no public constructors, companion factories,
helpers, removals, replacements, owner moves, or overload changes. Both rows
delegate to the already accepted bounded programmatic matcher, so the selected
candidate remains on the `0.2.0` source line.

The preceding bounded programmatic definition matcher review recorded exactly
12 additions and no removals:

- `DefinitionPattern` and its `singleParameter` factory;
- `DefinitionPatternError` and its sole `message` projection;
- `SingleParameterDefinitionPattern` and `matchDefinition`;
- generic `SingleParameterDefinitionMatch[Tpe, Trm]` and exactly its five
  projections: method name, parameter name, parameter type, result type, and
  body.

That matcher was additive and source-safe under the reviewed source shapes.

The preceding bounded reflected definition review recorded exactly one
addition and no removals:

- addition of `Quasiquotes.dqr(using q: Quotes)(args:
  q.reflect.TypeRepr*): q.reflect.DefDef`.

The prior bounded reflected type syntax review had recorded five additions:

- addition of `QuasiTypequotes.tqr(using q: Quotes)(args:
  q.reflect.TypeRepr*): q.reflect.TypeRepr`;
- addition of `QuasiTypequotes.tqq(using q: Quotes):
  TypePatternExtractor[q.reflect.TypeRepr]`;
- addition of public `TypePatternExtractor[T]` and its
  `unapplySeq(value: T): Option[Seq[T]]` protocol.
- addition of `QuasiTypequotes.tqr(templateSource: String, bindings:
  Seq[(String, TypeNormalForm)])`, with a distinct JVM target name.

The type sequence overload is a source-compatibility adapter: without it, adding the
same-named interpolator prevents Scala from eta-expanding the existing varargs
function under its previously supported `(String, Seq[(String,
TypeNormalForm)]) => Either[...]` expected type. Direct varargs calls remain
unchanged, and compile tests cover wildcard imports, selective imports,
ordinary calls, and method values. The `dqr` addition uses the already-public
`Quasiquotes.*` host and adds no overload to an existing name. Its Quotes-owned
result and bounded local-placement semantics remain explicitly experimental.
Regeneration against this accepted baseline must report
`NO_PUBLIC_API_DELTA`.

## Explicit non-guarantees

This tool is not a binary compatibility checker and cannot prove binary
compatibility, TASTy compatibility, source overload-resolution compatibility,
implicit/given search compatibility, semantic behavior compatibility,
compiler-internal compatibility, or runtime serialization compatibility.
