package quasiquotes.hybrid

/** Frozen experimental inventory. Rows describe the current public typed-Term
  * contract and its explicit exclusions; this unpublished value is executable
  * evidence, not a new public frontend API.
  */
private[quasiquotes] object TermQ3ParityMatrix:
  enum Classification derives CanEqual:
    case CURRENT_ENGINE_SUPPORTED
    case HYBRID_SCALAMETA_SUPPORTED
    case HYBRID_FALLBACK_REQUIRED
    case HYBRID_SEMANTIC_GAP
    case NOT_A_PUBLIC_TERM_CASE

  final case class Row(
      id: String,
      example: String,
      classification: Classification,
      evidence: String
  ) derives CanEqual

  import Classification.*

  val rows: Vector[Row] = Vector(
    Row("literal-int", "42", HYBRID_SCALAMETA_SUPPORTED, "construction and matching differential"),
    Row("literal-string", "\"value\"", HYBRID_SCALAMETA_SUPPORTED, "construction and matching differential"),
    Row("literal-boolean", "true", HYBRID_SCALAMETA_SUPPORTED, "construction and matching differential"),
    Row("identifier", "value", HYBRID_SCALAMETA_SUPPORTED, "Quotes-aware exact identifier resolution"),
    Row("selection", "value.size", HYBRID_SCALAMETA_SUPPORTED, "exact member-name selection"),
    Row("application", "f(x)", HYBRID_SCALAMETA_SUPPORTED, "typed application lowering"),
    Row("overloaded-application", "overloaded(1)", HYBRID_SCALAMETA_SUPPORTED, "Quotes chooses the applicable overload"),
    Row("infix", "left + right", HYBRID_SCALAMETA_SUPPORTED, "single-argument infix lowering"),
    Row("unary-prefix", "-value", HYBRID_SCALAMETA_SUPPORTED, "four admitted unary operators"),
    Row("term-hole", "$value", HYBRID_SCALAMETA_SUPPORTED, "exact caller-owned subtree identity"),
    Row("multiple-ordered-holes", "f($left, $right)", HYBRID_SCALAMETA_SUPPORTED, "ordered exact identities"),
    Row("placeholder-collision", "f(\"__qq_term_hole_0\", $value)", HYBRID_SCALAMETA_SUPPORTED, "categorized collision-safe synthesis"),
    Row("nested-expression", "f(g($value))", HYBRID_SCALAMETA_SUPPORTED, "recursive lowering and capture"),
    Row("ascription", "($value: Int)", HYBRID_SCALAMETA_SUPPORTED, "stable admitted type names"),
    Row("constructed-type-splice", "($value: $tpe)", HYBRID_SCALAMETA_SUPPORTED, "existing ConstructedType normal forms"),
    Row("tuple-2-to-22", "($left, $right)", HYBRID_SCALAMETA_SUPPORTED, "bounded tuple construction and matching"),
    Row("conditional", "if $condition then $left else $right", HYBRID_SCALAMETA_SUPPORTED, "typed If lowering and matching"),
    Row("s-interpolation", "s\"hello $name\"", HYBRID_SCALAMETA_SUPPORTED, "parts and ordered argument matching"),
    Row("layered-dollar-interpolation", "s\"hello $$name\"", HYBRID_SCALAMETA_SUPPORTED, "guest-dollar preservation"),
    Row("constructor-new", "new java.lang.StringBuilder($capacity)", HYBRID_SCALAMETA_SUPPORTED, "fully-qualified non-generic constructor"),
    Row("lambda1", "(x: Int) => x", HYBRID_SCALAMETA_SUPPORTED, "one typed ordinary alpha-aware binder"),
    Row("lambda1-body-hole", "(x: Int) => $body", HYBRID_SCALAMETA_SUPPORTED, "original body capture under binder scope"),
    Row("p1-expression-block", "{ effect1(); effect2(); result }", HYBRID_SCALAMETA_SUPPORTED, "ordered expression prefixes and distinct final result"),
    Row("ordered-extractor-captures", "($left, $right)", HYBRID_SCALAMETA_SUPPORTED, "capture order and identity"),
    Row("ordinary-mismatch", "42 against 1", HYBRID_SCALAMETA_SUPPORTED, "None-equivalent matcher failure"),
    Row("generated-nospan", "$whole", HYBRID_SCALAMETA_SUPPORTED, "source-free target capture identity"),
    Row("malformed-template", "$value +", HYBRID_SCALAMETA_SUPPORTED, "controlled located diagnostic"),
    Row("repeated-named-hole-equal", "($x, $x)", HYBRID_SCALAMETA_SUPPORTED, "normalized repeated capture equality"),
    Row("repeated-named-hole-mismatch", "($x, $x)", HYBRID_SCALAMETA_SUPPORTED, "controlled repeated capture mismatch"),
    Row("source-provenance", "independently authored target", HYBRID_SCALAMETA_SUPPORTED, "original reflected subtree is returned"),
    Row("local-values-and-definitions", "{ val x = 1; x }", NOT_A_PUBLIC_TERM_CASE, "P2/P3 block exclusion"),
    Row("match-try-loops-for", "value match { case _ => 1 }", NOT_A_PUBLIC_TERM_CASE, "documented exclusion"),
    Row("generic-or-unqualified-constructor", "new Box[Int](1)", NOT_A_PUBLIC_TERM_CASE, "constructor tranche exclusion"),
    Row("multi-parameter-or-nested-lambda", "(x: Int, y: Int) => x", NOT_A_PUBLIC_TERM_CASE, "Lambda1 exclusion"),
    Row("non-s-interpolation", "raw\"value\"", NOT_A_PUBLIC_TERM_CASE, "interpolation tranche exclusion"),
    Row("general-ast-quasiquote", "arbitrary scala.meta.Tree", NOT_A_PUBLIC_TERM_CASE, "not a typed Term contract")
  )

  val requiredIds: Set[String] = rows.map(_.id).toSet
