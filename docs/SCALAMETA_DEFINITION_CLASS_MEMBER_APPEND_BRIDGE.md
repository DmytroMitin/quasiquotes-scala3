# Bounded Scalameta Definition class-member append bridge

`ScalametaDefinitionClassMemberAppendBridge` is a public exact-compiler-version
operation in the remotely unpublished `dottyInternal` module. It composes two
existing authorities without replacing either:

```text
supported scala.meta.Defn
  -> ScalametaDefinitionGeneratedOriginBridge
  -> positioned generated DefDef or ValDef

existing admitted pre-Typer ordinary class + that exact member
  -> existing-tree append authority
  -> rebuilt class with the member appended last
```

## Programmatic use

An active Dotty compiler `Context` and a parser-produced pre-Typer class are
required:

```scala
import scala.meta.*
import scala.meta.dialects.Scala3
import quasiquotes.definitions.dotty.ScalametaDefinitionClassMemberAppendBridge

val definition = Scala3("def foo(x: Int): String = x.toString")
  .parse[Stat]
  .get
  .asInstanceOf[Defn]

val lowered = ScalametaDefinitionClassMemberAppendBridge.append(
  existingClass,
  definition,
  "<generated:add-foo>"
)
```

On success, `lowered.tree` is the fresh replacement class,
`lowered.appendedMember` is the exact final body object,
`lowered.generatedSource` is deterministic generated output, and
`lowered.generatedSourceFile` is the generated member's virtual source.

The admitted generated families are exactly the generic generated-origin
Definition bridge's four concrete families: explicitly typed immutable `val`,
true parameterless explicitly typed `def`, one-ordinary-parameter explicitly
typed `def`, and exact-two-ordinary-parameter explicitly typed `def`. A simple
Type alias and every broader Definition family fail before existing-class
reconstruction.

## Mixed provenance and identity

The rebuilt graph deliberately contains three provenance kinds:

- every old direct member is the exact original object in original order and
  retains its original source/span;
- the appended member is the exact generated object and retains its generated
  virtual `SourceFile` and recursive spans;
- only the enclosing Template and class shells are fresh, using the existing
  class's truthful same-site replacement source/span.

The bridge does not render or reparse the class, clone or reposition the new
member, or copy the class source onto it. It appends exactly one member at the
final direct-body position. Sixty-three old direct members plus the new member
is the maximum successful body; an already full 64-member body rejects the
append atomically.

## Failures

`append` returns `Either[Failure, Lowered]`. The stable public stage codes are:

- `GENERATED_DEFINITION_FAILED` — projection, family, completion, generated
  origin, missing Definition, or virtual-source rejection from the existing
  generated Definition bridge;
- `EXISTING_CLASS_APPEND_FAILED` — existing-class admission, pre-Typer,
  provenance, capacity, exact-identity, or reconstruction rejection from the
  existing append authority.

The detail begins with the upstream stable code, so callers can retain useful
diagnostics without depending on private error ADTs. The method does not catch
arbitrary compiler exceptions or return partial output.

## Ownership boundary

These responsibilities remain distinct:

```text
new-definition projection/authoring and exact lowering  = N/C/U-D
existing-tree capture and reconstruction                = U-U
hybrid source-facing composition                        = C
target admission, plugin lifecycle, placement, rollback = Macro-Paradise/caller
```

This API is not public `u*` quasiquote syntax, general class/trait/object
authoring, a universal existing-tree editor, owner/symbol repair, post-Typer
rewriting, multi-member append, or arbitrary insertion.
