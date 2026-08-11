# Public API inventory and exact-shape diff

This standalone, standard-library-only tool generates the deterministic
`core`/`frontend` Scaladoc inventory and compares exact API shapes. It adds no
runtime dependency to any Scala module.

## Recommended current-minor gate

First create the stable documentation JARs:

```text
sbt -batch core/packageDoc frontend/packageDoc
```

Then run:

```text
tools/public-api/check-current.sh BASELINE CORE_DOCS_JAR FRONTEND_DOCS_JAR OUTPUT_DIR current-minor
```

The output directory contains `candidate.tsv`, `delta.tsv`, and `summary.txt`.
The baseline and candidate inventory schema is exactly:

```text
module  owner  kind  name  signature
```

The logical group key is `(module, owner, kind, name)` and members are exact
signatures. Input order is irrelevant; exact duplicate rows, bad headers,
invalid field counts, unsupported modules/kinds, or empty identity fields fail
closed. Owner, module, or kind moves remain exact removal plus addition facts.
No fuzzy move inference is performed.

Exit codes for `current-minor` mode are:

- `0`: no API shape delta;
- `2`: additive-only shape delta, explicit human review required;
- `3`: a removal/breaking shape requires a new experimental 0.x minor;
- `4`: malformed or unsupported inventory.

`report` mode emits the same deterministic artifacts and returns zero for any
valid delta; malformed input still returns `4`.

## Limits

This is review automation, not a binary compatibility checker. It cannot prove
binary or TASTy compatibility, source overload-resolution compatibility,
implicit/given search compatibility, semantic behavior compatibility,
compiler-internal compatibility, or runtime serialization compatibility.
Additive overloads, givens, and extensions therefore remain human-review
required and are never automatically declared patch-safe.
