package rankedselectedconsumer

import scala.quoted.*
import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.QuasiPattern.*

/** Independently compiled foreign-package consumer of the standard public qq. */
object Q083PublicRankedConsumer:
  object root:
    object first:
      def second(): Int = 0
      def second(value: Int): Int = value
      def second(a: Int, b: Int, c: Int): Int = a + b + c
      def curried(a: Int)(b: Int): Int = a + b
      def contextual(using value: Int): Int = value
      def multi(value: Int)(using other: Int): Int = value + other
      def generic[A](value: A): A = value
      infix def combine(value: Int): Int = value
      def +:(value: Int): Int = value
      def `+`(value: Int): Int = value
      def `type`(value: Int): Int = value
      def `safe spaced name`(value: Int): Int = value
      def `__qqhole_first__selected_rank2_1`(value: Int): Int = value
      def naïve(value: Int): Int = value
      val bare: Int = 1
      val function: Int => Int = value => value
    object same:
      def same(value: Int): Int = value
    object naïve:
      def second(value: Int): Int = value
  object longer:
    object middle:
      object first:
        def second(value: Int): Int = value
  object repeatedApply:
    object apply:
      val apply: Int => Int = value => value

  transparent inline def wrapped = root.first.second(1)

  inline def observations: List[String] = ${ observationsImpl }
  inline def lexicalObservations: List[String] = ${ lexicalImpl }
  inline def negativeObservations: List[String] = ${ negativesImpl }
  inline def reviewObservations: List[String] = ${ reviewImpl }

  def capture(using q: Quotes)(target: q.reflect.Term): Option[
    (q.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q.reflect.Term])
  ] = target match
    case qq"$receiver.$first.$second(..$arguments)" =>
      val r: q.reflect.Term = receiver
      val a: SelectedMemberName = first
      val b: SelectedMemberName = second
      val as: Seq[q.reflect.Term] = arguments
      Some((r, a, b, as))
    case _ => None

  private def strip(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => strip(body)
      case other => other

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    def check(label: String, raw: Term): String =
      val target = strip(raw)
      val (expectedReceiver, expectedArguments) = target match
        case Apply(Select(Select(receiver, _), _), arguments) => (receiver, arguments)
        case _ => report.errorAndAbort(s"invalid Q083 direct-call fixture $label: ${target.show(using Printer.TreeStructure)}")
      val (receiver, first, second, arguments) = capture(target).getOrElse(
        report.errorAndAbort(s"public Q083 did not match $label"))
      val identity = (receiver eq expectedReceiver) && receiver.symbol == expectedReceiver.symbol &&
        receiver.symbol.owner == expectedReceiver.symbol.owner &&
        arguments.size == expectedArguments.size && arguments.zip(expectedArguments).forall { (a, b) =>
          (a eq b) && a.symbol == b.symbol && (!a.symbol.exists || a.symbol.owner == b.symbol.owner)
        }
      if !identity then report.errorAndAbort(s"Q083 raw Term identity mismatch: $label")
      s"$label:${first.decoded}:${second.decoded}:${arguments.size}:identity=$identity"
    val zero = check("zero", '{ root.first.second() }.asTerm)
    val one = check("one", '{ root.first.second(1) }.asTerm)
    val many = check("many", '{ root.first.second(1, 2, 3) }.asTerm)
    // A local val is intentionally a raw Ident, although the legacy target
    // view may follow its RHS. This catches normalization-based captures.
    val local = '{ val value: Int = 9; root.first.second(value) }.asTerm
    val localCall = strip(local) match
      case Block(_, call) => call
      case _ => report.errorAndAbort("Q083 local argument fixture lost its block")
    val localIdentity = check("local", localCall)
    val localReceiver = strip('{ val receiver: root.type = root; receiver.first.second(1) }.asTerm) match
      case Block(_, call) => call
      case _ => report.errorAndAbort("Q083 local receiver fixture lost its block")
    val localReceiverIdentity = check("local-receiver", localReceiver)
    val suffix = check("suffix", '{ longer.middle.first.second(1) }.asTerm)
    val suffixTarget = strip('{ longer.middle.first.second(1) }.asTerm)
    val fixedRoot = strip('{ root }.asTerm).symbol
    val baseAnchored = strip('{ root.first.second(1) }.asTerm) match
      case qq"$receiver.$first.$second(..$arguments)" if receiver.symbol == fixedRoot => true
      case _ => false
    val anchored = suffixTarget match
      case qq"$receiver.$first.$second(..$arguments)" if receiver.symbol == fixedRoot => true
      case _ => false
    val wrappedIdentity = check("wrapped", Inlined(None, Nil, strip('{ root.first.second(1) }.asTerm)))
    Expr.ofList(List(zero, one, many, localIdentity, localReceiverIdentity, suffix, s"fixed-root-positive:$baseAnchored",
      s"fixed-root-rejected:${!anchored}", wrappedIdentity).map(Expr(_)))

  private def lexicalImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    val values = List(
      "equal" -> '{ root.same.same(1) }.asTerm,
      "operator" -> '{ root.first.`+`(1) }.asTerm,
      "keyword" -> '{ root.first.`type`(1) }.asTerm,
      "spaced" -> '{ root.first.`safe spaced name`(1) }.asTerm,
      "generated" -> '{ root.first.`__qqhole_first__selected_rank2_1`(1) }.asTerm,
      "unicode-first" -> '{ root.naïve.second(1) }.asTerm,
      "unicode-second" -> '{ root.first.naïve(1) }.asTerm
    ).map { (label, target) =>
      capture(strip(target)) match
        case Some((_, first, second, _)) => s"$label:${first.decoded}:${second.decoded}"
        case None => s"$label:fallthrough"
    }
    Expr.ofList(values.map(Expr(_)))

  private def reviewImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    val positives = List(
      "qualifier-parens" -> '{ (root.first).second(1) }.asTerm,
      "function-parens" -> '{ (root.first.second)(1) }.asTerm,
      "both-parens" -> '{ ((root.first).second)(1) }.asTerm,
      "explicit-apply" -> '{ root.first.function.apply(1) }.asTerm,
      "explicit-equal-apply" -> '{ repeatedApply.apply.apply.apply(1) }.asTerm
    ).map { (label, raw) =>
      val target = strip(raw)
      val identity = (target, capture(target)) match
        case (Apply(Select(Select(expectedReceiver, _), _), expectedArguments),
              Some((receiver, _, _, arguments))) =>
          (receiver eq expectedReceiver) && arguments.size == expectedArguments.size &&
            arguments.zip(expectedArguments).forall((a,b) => a eq b)
        case _ => false
      s"$label:identity=$identity"
    }
    val implicitApply = capture(strip('{ root.first.function(1) }.asTerm)).isEmpty
    val implicitEqualApply = capture(strip('{ repeatedApply.apply.apply(1) }.asTerm)).isEmpty
    Expr.ofList((positives ++ List(s"implicit-apply-rejected:$implicitApply",
      s"implicit-equal-apply-rejected:$implicitEqualApply")).map(Expr(_)))

  private def negativesImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    val call = strip('{ root.first.second(1) }.asTerm)
    val fixtures = List(
      "bare" -> '{ root.first.bare }.asTerm,
      "curried" -> '{ root.first.curried(1)(2) }.asTerm,
      "contextual" -> '{ root.first.contextual(using 2) }.asTerm,
      "multi" -> '{ root.first.multi(1)(using 2) }.asTerm,
      "type-apply" -> '{ root.first.generic[Int](1) }.asTerm,
      "named" -> '{ root.first.second(value = 1) }.asTerm,
      "infix" -> '{ root.first combine 1 }.asTerm,
      "right-infix" -> '{ 1 +: root.first }.asTerm,
      "inline-ident" -> '{ wrapped }.asTerm,
      "block" -> Block(Nil, call),
      "typed" -> Typed(call, TypeTree.of[Int])
    )
    val values = fixtures.map((label, target) => s"$label:${capture(strip(target)).isEmpty}") :+
      s"null:${capture(new java.util.HashMap[String, Term]().get("missing")).isEmpty}"
    Expr.ofList(values.map(Expr(_)))
