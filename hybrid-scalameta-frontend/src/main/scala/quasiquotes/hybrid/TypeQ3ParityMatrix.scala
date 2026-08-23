package quasiquotes.hybrid

/** Executable inventory of the current public typed-Type contract and its
  * explicit exclusions. This is unpublished evidence, not a public frontend.
  */
private[quasiquotes] object TypeQ3ParityMatrix:
  enum Classification derives CanEqual:
    case CURRENT_PUBLIC_TYPE_CASE
    case NOT_A_PUBLIC_TYPE_CASE
    case DEFERRED_TYPE_FAMILY

  final case class Row(
      id: String,
      example: String,
      classification: Classification,
      evidence: String
  ) derives CanEqual

  import Classification.*

  val rows: Vector[Row] = Vector(
    Row("named-types", "Int", CURRENT_PUBLIC_TYPE_CASE, "Int, String, and Boolean normal forms"),
    Row("fixed-unary-applied", "List[Int]", CURRENT_PUBLIC_TYPE_CASE, "List and Option arity-one recursion"),
    Row("fixed-binary-applied", "Either[Int, String]", CURRENT_PUBLIC_TYPE_CASE, "Either arity-two recursion"),
    Row("recursive-fixed-applied", "Either[List[Int], Option[String]]", CURRENT_PUBLIC_TYPE_CASE, "nested admitted constructors"),
    Row("tuple2", "(Int, String)", CURRENT_PUBLIC_TYPE_CASE, "bounded tuple construction and matching"),
    Row("tuple3", "(Int, String, Boolean)", CURRENT_PUBLIC_TYPE_CASE, "bounded tuple construction and matching"),
    Row("function1", "Int => String", CURRENT_PUBLIC_TYPE_CASE, "bounded function construction and matching"),
    Row("function2", "(Int, String) => Boolean", CURRENT_PUBLIC_TYPE_CASE, "bounded function construction and matching"),
    Row("programmatic-template-holes", "Either[$left, $right]", CURRENT_PUBLIC_TYPE_CASE, "core TypeTemplate bindings"),
    Row("programmatic-pattern-holes", "Either[$left, $right]", CURRENT_PUBLIC_TYPE_CASE, "core TypePattern bindings"),
    Row("repeated-programmatic-hole-equality", "Either[$same, $same]", CURRENT_PUBLIC_TYPE_CASE, "structural repeated-hole equality"),
    Row("tqr-zero-slot", "tqr\"Int\"", CURRENT_PUBLIC_TYPE_CASE, "zero-splice reflected construction"),
    Row("tqr-one-slot", "tqr\"List[$element]\"", CURRENT_PUBLIC_TYPE_CASE, "one inspected reflected splice"),
    Row("tqr-multiple-slots", "tqr\"Either[$left, $right]\"", CURRENT_PUBLIC_TYPE_CASE, "ordered inspected reflected splices"),
    Row("tqq-zero-slot", "tqq\"Int\"", CURRENT_PUBLIC_TYPE_CASE, "zero-capture reflected matching"),
    Row("tqq-one-capture", "tqq\"List[$element]\"", CURRENT_PUBLIC_TYPE_CASE, "one original reflected capture"),
    Row("tqq-multiple-captures", "tqq\"Either[$left, $right]\"", CURRENT_PUBLIC_TYPE_CASE, "ordered original reflected captures"),
    Row("tqq-original-subtree-identity", "nested target", CURRENT_PUBLIC_TYPE_CASE, "path-indexed original TypeRepr return"),
    Row("tqq-mismatch-fallthrough", "ordinary mismatch", CURRENT_PUBLIC_TYPE_CASE, "None-equivalent extractor result"),
    Row("generated-source-poor-target", "compiler-generated TypeRepr", CURRENT_PUBLIC_TYPE_CASE, "identity does not depend on source spans"),
    Row("malformed-located-diagnostics", "List[", CURRENT_PUBLIC_TYPE_CASE, "controlled categorized parse location"),
    Row("semantic-equality-subtyping", "A =:= B", NOT_A_PUBLIC_TYPE_CASE, "separate semantic capability gate"),
    Row("type-constructor-holes", "$constructor[Int]", NOT_A_PUBLIC_TYPE_CASE, "not admitted by the public type contract"),
    Row("neutral-ntqr-ntqq", "ntqr/ntqq", NOT_A_PUBLIC_TYPE_CASE, "neutral frontend has no public type spellings"),
    Row("selected-path-dependent", "pkg.Type", DEFERRED_TYPE_FAMILY, "requires resolver and path policy"),
    Row("wildcards-and-bounds", "List[? <: AnyVal]", DEFERRED_TYPE_FAMILY, "wildcard and bound semantics deferred"),
    Row("refinements", "A { type T = Int }", DEFERRED_TYPE_FAMILY, "refinement semantics deferred"),
    Row("match-types", "T match { case Int => String }", DEFERRED_TYPE_FAMILY, "match-type semantics deferred"),
    Row("broader-constructors-and-arities", "Map[Int, String]", DEFERRED_TYPE_FAMILY, "fixed constructor and arity boundary"),
    Row("unions-intersections-and-advanced-types", "A | B", DEFERRED_TYPE_FAMILY, "broader Scala 3 type grammar deferred")
  )

  val requiredIds: Set[String] = rows.map(_.id).toSet
