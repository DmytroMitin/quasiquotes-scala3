package quasiquotes

import scala.quoted.Quotes

/** Test-only check of whether direct exports retain interpolation semantics. */
object PlainExportFacadeProbe:
  export construct.Quasiquotes.{dqr, qr}
  export matching.DefinitionPattern.dqq
  export matching.QuasiPattern.qq
  export types.QuasiTypequotes.{tqq, tqr}

/** Test-only candidate for one additive umbrella import. */
object FacadeProbe:
  extension (sc: StringContext)
    def qr(using q: Quotes)(
        args: (q.reflect.Term | q.reflect.TypeRepr | construct.QuasiTypeSplice | construct.SelectedMemberName)*
    ): q.reflect.Term =
      construct.Quasiquotes.qr(sc)(using q)(args*)

    def qq(using q: Quotes) = matching.QuasiPattern.qq(sc)(using q)

    def tqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.TypeRepr =
      types.QuasiTypequotes.tqr(sc)(using q)(args*)

    def tqq(using q: Quotes) = types.QuasiTypequotes.tqq(sc)(using q)

    def dqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.DefDef =
      construct.Quasiquotes.dqr(sc)(using q)(args*)

    def dqq(using q: Quotes) = matching.DefinitionPattern.dqq(sc)(using q)

package domains:
  /** Test-only candidate for narrower, domain-oriented imports. */
  object Terms:
    extension (sc: StringContext)
      def qr(using q: Quotes)(
          args: (q.reflect.Term | q.reflect.TypeRepr | quasiquotes.construct.QuasiTypeSplice | quasiquotes.construct.SelectedMemberName)*
      ): q.reflect.Term =
        quasiquotes.construct.Quasiquotes.qr(sc)(using q)(args*)

      def qq(using q: Quotes) = quasiquotes.matching.QuasiPattern.qq(sc)(using q)

  /** Test-only candidate for narrower, domain-oriented imports. */
  object Types:
    extension (sc: StringContext)
      def tqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.TypeRepr =
        quasiquotes.types.QuasiTypequotes.tqr(sc)(using q)(args*)

      def tqq(using q: Quotes) = quasiquotes.types.QuasiTypequotes.tqq(sc)(using q)

  /** Test-only candidate for narrower, domain-oriented imports. */
  object Definitions:
    extension (sc: StringContext)
      def dqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.DefDef =
        quasiquotes.construct.Quasiquotes.dqr(sc)(using q)(args*)

      def dqq(using q: Quotes) = quasiquotes.matching.DefinitionPattern.dqq(sc)(using q)
