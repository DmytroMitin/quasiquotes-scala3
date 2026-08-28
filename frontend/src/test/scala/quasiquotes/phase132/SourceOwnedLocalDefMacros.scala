package quasiquotes.phase132

import scala.quoted.*

import quasiquotes.construct.Quasiquotes.qr
import quasiquotes.construct.{QuasiquoteBuilder, QuasiTypeSplices, SelectedMemberName}
import quasiquotes.types.{ConstructedType, TypeNormalForm}

object SourceOwnedLocalDefMacros:
  inline def identity(inline value: Int): Int = ${ identityImpl('value) }

  inline def ownerAndHygieneEvidence(inline value: Int): (Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) =
    ${ ownerAndHygieneEvidenceImpl('value) }

  inline def freshPerExpansion: Boolean = ${ freshPerExpansionImpl }

  inline def rejectionEvidence: List[String] = ${ rejectionEvidenceImpl }

  private def identityImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    val parameterType = TypeRepr.of[Int]
    val resultType = TypeRepr.of[Int]
    qr"""{
      def boundedIdentity(value: $parameterType): $resultType = value
      boundedIdentity(${value.asTerm})
    }""".asExprOf[Int]

  private def ownerAndHygieneEvidenceImpl(
      value: Expr[Int]
  )(using q: Quotes): Expr[(Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)] =
    import q.reflect.*
    val argument = value.asTerm
    val parameterType = TypeRepr.of[Int]
    val resultType = TypeRepr.of[Int]
    val built = qr"""{
      def boundedIdentity(value: $parameterType): $resultType = value
      boundedIdentity($argument)
    }"""

    built match
      case Block(List(definition: DefDef), Apply(methodReference, List(loweredArgument))) =>
        val parameter = definition.termParamss.flatMap(_.params).head
        val rhsReference = definition.rhs.get
        Expr(
          (
            definition.symbol.exists && definition.symbol.owner == Symbol.spliceOwner,
            definition.symbol != parameter.symbol,
            parameter.symbol.owner == definition.symbol,
            rhsReference.symbol == parameter.symbol,
            methodReference.symbol == definition.symbol,
            loweredArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])
          )
        )
      case other =>
        report.errorAndAbort(
          s"expected one source-owned DefDef and bound call, obtained ${other.show(using Printer.TreeStructure)}"
        )

  private def freshPerExpansionImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val parameterType = TypeRepr.of[Int]
    val resultType = TypeRepr.of[Int]

    def build(): Term =
      qr"""{
        def boundedIdentity(value: $parameterType): $resultType = value
        boundedIdentity(${Expr(1).asTerm})
      }"""

    val first = build()
    val second = build()
    val distinct = (first, second) match
      case (Block(List(left: DefDef), _), Block(List(right: DefDef), _)) =>
        left.symbol != right.symbol
      case _ => false
    Expr(distinct)

  private def rejectionEvidenceImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    def outcome(
        parts: Seq[String],
        arguments: Seq[Term | TypeRepr | quasiquotes.construct.QuasiTypeSplice | SelectedMemberName]
    ): String =
      QuasiquoteBuilder.build(parts, arguments).fold(_.message, _ => "accepted")

    val intType = TypeRepr.of[Int]
    val stringType = TypeRepr.of[String]
    val constructedInt = QuasiTypeSplices.typeSplice(
      ConstructedType(TypeNormalForm.STypeIdent("Int"))
    )
    val dynamicName = SelectedMemberName.from("dynamicIdentity").toOption.get
    val messages = List(
      outcome(Seq("{ def first(value: Int): Int = value; def second(value: Int): Int = value; first(1) }"), Nil),
      outcome(Seq("{ inline def id(value: Int): Int = value; id(1) }"), Nil),
      outcome(Seq("{ def id[A](value: Int): Int = value; id(1) }"), Nil),
      outcome(Seq("{ def id(value: Int)(other: Int): Int = value; id(1)(2) }"), Nil),
      outcome(Seq("{ def id(value: Int): Int = 1; id(1) }"), Nil),
      outcome(Seq("{ def id(value: Int): Int = id(value); id(1) }"), Nil),
      outcome(Seq("{ def id(value: ", "): ", " = value; id(1) }"), Seq(Expr(1).asTerm, intType)),
      outcome(Seq("{ def id(value: List[", "]): Int = value; id(Nil) }"), Seq(intType)),
      outcome(Seq("{ def id(value: ", "): ", " = value; id(\"x\") }"), Seq(stringType, intType)),
      outcome(Seq("{ def ", "(value: Int): Int = value; dynamicIdentity(1) }"), Seq(dynamicName)),
      outcome(Seq("{ def id(value: Int) = value; id(1) }"), Nil),
      outcome(Seq("{ def id(value: List[Int]): Int = value; id(Nil) }"), Nil),
      outcome(Seq("{ def id(value: ", "): Int = value; id(1) }"), Seq(constructedInt)),
      outcome(Seq("{ def id(value: => Int): Int = value; id(1) }"), Nil)
    )
    Expr.ofList(messages.map(Expr(_)))
