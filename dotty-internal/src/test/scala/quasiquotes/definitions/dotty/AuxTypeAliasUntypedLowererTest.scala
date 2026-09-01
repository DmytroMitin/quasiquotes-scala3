package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.Param
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.parser.BinderId
import AuxTypeAliasUntypedLoweringInput.*
import AuxTypeAliasUntypedLoweringInput.TypeInput.*

class AuxTypeAliasUntypedLowererTest extends munit.FunSuite:
  test("canonical validated input lowers to the exact source-free 18-node alias") {
    withContext {
      val validated = AuxTypeAliasUntypedLoweringInput
        .validate(canonicalDescription())
        .fold(problem => fail(problem.message), identity)
      val raw = AuxTypeAliasUntypedLowerer
        .lower(validated)
        .fold(problem => fail(problem.message), identity)

      assertEquals(
        structure(raw),
        "TypeDef(Aux,0,TypeLambda(List(TypeDef(N,259,TypeBounds(Empty,Ident(Nat),Empty)), TypeDef(M,259,TypeBounds(Empty,Ident(Nat),Empty)), TypeDef(Out0,259,TypeBounds(Empty,Ident(Nat),Empty))),Refined(Applied(Ident(Add),List(Ident(N), Ident(M))),List(TypeDef(Out,0,Ident(Out0))))))"
      )
      val trees = allTrees(raw)
      assertEquals(trees.size, 18)
      trees.foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("near-miss descriptions fail closed at their owned validation boundary") {
    val canonical = canonicalDescription()
    val first = canonical.parameters(0)
    val second = canonical.parameters(1)
    val output = canonical.parameters(2)
    val target = canonical.target.asInstanceOf[Applied]
    val member = canonical.refinements.head.asInstanceOf[DirectTypeAlias]

    val cases = Vector(
      "two parameters" ->
        (canonical.copy(parameters = canonical.parameters.take(2)), "TYPE_PARAMETER_ARITY_UNSUPPORTED"),
      "four parameters" ->
        (canonical.copy(parameters = canonical.parameters :+ output.copy(binderId = BinderId(4), displayName = "Extra")), "TYPE_PARAMETER_ARITY_UNSUPPORTED"),
      "duplicate binder" ->
        (canonical.copy(parameters = canonical.parameters.updated(1, second.copy(binderId = first.binderId))), "TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT"),
      "duplicate display name" ->
        (canonical.copy(parameters = canonical.parameters.updated(1, second.copy(displayName = first.displayName))), "TYPE_PARAMETER_NAMES_MUST_BE_DISTINCT"),
      "missing upper bound" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(upperBound = None))), "TYPE_PARAMETER_BOUND_UNSUPPORTED"),
      "lower and upper bound" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(lowerBound = Some(SourceName("Nothing"))))), "TYPE_PARAMETER_BOUND_UNSUPPORTED"),
      "covariant parameter" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(variance = Variance.Covariant))), "TYPE_PARAMETER_VARIANCE_UNSUPPORTED"),
      "higher-kinded parameter" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(nestedTypeParameters = Vector(output)))), "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED"),
      "context bound" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(contextBounds = Vector(SourceName("Ordering"))))), "TYPE_PARAMETER_CONTEXT_BOUNDS_UNSUPPORTED"),
      "view bound" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(viewBounds = Vector(SourceName("String"))))), "TYPE_PARAMETER_VIEW_BOUNDS_UNSUPPORTED"),
      "qualified upper bound" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(upperBound = Some(Qualified("pkg", "Nat"))))), "TYPE_PARAMETER_BOUND_UNSUPPORTED"),
      "applied upper bound" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(upperBound = Some(Applied(SourceName("Box"), Vector(SourceName("Nat"))))))), "TYPE_PARAMETER_BOUND_UNSUPPORTED"),
      "one target argument" ->
        (canonical.copy(target = target.copy(arguments = target.arguments.take(1))), "TARGET_ARITY_UNSUPPORTED"),
      "three target arguments" ->
        (canonical.copy(target = target.copy(arguments = target.arguments :+ BinderReference(output.binderId, output.displayName))), "TARGET_ARITY_UNSUPPORTED"),
      "qualified target constructor" ->
        (canonical.copy(target = target.copy(constructor = Qualified("pkg", "Add"))), "TARGET_CONSTRUCTOR_UNSUPPORTED"),
      "nested target argument" ->
        (canonical.copy(target = target.copy(arguments = target.arguments.updated(0, Applied(SourceName("Box"), Vector(target.arguments.head))))), "TARGET_ARGUMENT_TOPOLOGY_UNSUPPORTED"),
      "reversed binder arguments" ->
        (canonical.copy(target = target.copy(arguments = target.arguments.reverse)), "TARGET_BINDER_REFERENCE_MISMATCH"),
      "duplicate first binder argument" ->
        (canonical.copy(target = target.copy(arguments = Vector(target.arguments.head, target.arguments.head))), "TARGET_BINDER_REFERENCE_MISMATCH"),
      "wrong target binder identity with expected display name" ->
        (canonical.copy(target = target.copy(arguments = target.arguments.updated(0, BinderReference(BinderId(99), first.displayName)))), "TARGET_BINDER_REFERENCE_MISMATCH"),
      "zero refinement members" ->
        (canonical.copy(refinements = Vector.empty), "REFINEMENT_CARDINALITY_UNSUPPORTED"),
      "two refinement members" ->
        (canonical.copy(refinements = Vector(member, member.copy(memberName = "Other"))), "REFINEMENT_CARDINALITY_UNSUPPORTED"),
      "abstract refinement member" ->
        (canonical.copy(refinements = Vector(AbstractTypeAlias("Out", None, None))), "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"),
      "parameterized refinement alias" ->
        (canonical.copy(refinements = Vector(member.copy(typeParameters = Vector(output)))), "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"),
      "bounded refinement alias" ->
        (canonical.copy(refinements = Vector(member.copy(upperBound = Some(SourceName("Nat"))))), "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"),
      "modified refinement alias" ->
        (canonical.copy(refinements = Vector(member.copy(modifiers = Vector("opaque")))), "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"),
      "refinement RHS uses binder one" ->
        (canonical.copy(refinements = Vector(member.copy(rhs = BinderReference(first.binderId, first.displayName)))), "REFINEMENT_RHS_BINDER_MISMATCH"),
      "refinement RHS uses binder two" ->
        (canonical.copy(refinements = Vector(member.copy(rhs = BinderReference(second.binderId, second.displayName)))), "REFINEMENT_RHS_BINDER_MISMATCH"),
      "wrong refinement binder identity with expected display name" ->
        (canonical.copy(refinements = Vector(member.copy(rhs = BinderReference(BinderId(99), output.displayName)))), "REFINEMENT_RHS_BINDER_MISMATCH"),
      "refinement RHS is arbitrary source Type" ->
        (canonical.copy(refinements = Vector(member.copy(rhs = SourceName("Other")))), "REFINEMENT_RHS_TOPOLOGY_UNSUPPORTED"),
      "invalid refinement member name" ->
        (canonical.copy(refinements = Vector(member.copy(memberName = "bad.name"))), "REFINEMENT_MEMBER_NAME_INVALID")
    )

    cases.foreach { case (label, (description, expectedCode)) =>
      assertEquals(
        AuxTypeAliasUntypedLoweringInput.validate(description).left.toOption.map(_.code),
        Some(expectedCode),
        label
      )
    }
  }

  test("null descriptions, members, names, and references fail without compiler exceptions") {
    val canonical = canonicalDescription()
    val target = canonical.target.asInstanceOf[Applied]
    val member = canonical.refinements.head.asInstanceOf[DirectTypeAlias]
    val first = canonical.parameters.head

    val cases = Vector(
      "null description" ->
        (null.asInstanceOf[Description], "VALIDATED_INPUT_REQUIRED"),
      "null alias name" ->
        (canonical.copy(aliasName = null), "ALIAS_NAME_INVALID"),
      "null parameters" ->
        (canonical.copy(parameters = null), "TYPE_PARAMETER_ARITY_UNSUPPORTED"),
      "null parameter name" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(displayName = null))), "TYPE_PARAMETER_NAME_INVALID"),
      "null lower-bound option" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(lowerBound = null))), "TYPE_PARAMETER_BOUND_UNSUPPORTED"),
      "null upper-bound option" ->
        (canonical.copy(parameters = canonical.parameters.updated(0, first.copy(upperBound = null))), "TYPE_PARAMETER_BOUND_UNSUPPORTED"),
      "null target" ->
        (canonical.copy(target = null), "TARGET_TOPOLOGY_UNSUPPORTED"),
      "null target constructor" ->
        (canonical.copy(target = target.copy(constructor = null)), "TARGET_CONSTRUCTOR_UNSUPPORTED"),
      "null target reference" ->
        (canonical.copy(target = target.copy(arguments = target.arguments.updated(0, null))), "TARGET_ARGUMENT_TOPOLOGY_UNSUPPORTED"),
      "null refinements" ->
        (canonical.copy(refinements = null), "REFINEMENT_CARDINALITY_UNSUPPORTED"),
      "null refinement" ->
        (canonical.copy(refinements = Vector(null)), "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"),
      "null refinement lower-bound option" ->
        (canonical.copy(refinements = Vector(member.copy(lowerBound = null))), "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"),
      "null refinement upper-bound option" ->
        (canonical.copy(refinements = Vector(member.copy(upperBound = null))), "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"),
      "null refinement RHS" ->
        (canonical.copy(refinements = Vector(member.copy(rhs = null))), "REFINEMENT_RHS_TOPOLOGY_UNSUPPORTED")
    )

    cases.foreach { case (label, (description, expectedCode)) =>
      assertEquals(
        AuxTypeAliasUntypedLoweringInput.validate(description).left.toOption.map(_.code),
        Some(expectedCode),
        label
      )
    }
    withContext {
      assertEquals(
        AuxTypeAliasUntypedLowerer.lower(null).left.toOption.map(_.code),
        Some("VALIDATED_INPUT_REQUIRED")
      )
      assertEquals(
        AuxTypeAliasGeneratedOriginAdapter
          .lower(null, "generated/NullValidatedAuxAlias.scala")
          .left
          .toOption
          .map(_.code),
        Some("VALIDATED_INPUT_REQUIRED")
      )
    }
  }

  test("generated origin is deterministic, complete, contained, and distinct from source-free lowering") {
    withContext {
      val validated = AuxTypeAliasUntypedLoweringInput
        .validate(canonicalDescription())
        .fold(problem => fail(problem.message), identity)
      val first = AuxTypeAliasGeneratedOriginAdapter
        .lower(validated, "<quasiquotes-generated:u001-aux-alias>")
        .fold(problem => fail(problem.message), identity)
      val second = AuxTypeAliasGeneratedOriginAdapter
        .lower(validated, "<quasiquotes-generated:u001-aux-alias>")
        .fold(problem => fail(problem.message), identity)

      val expected =
        "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }"
      assertEquals(first.generatedSource, expected)
      assertEquals(second.generatedSource, expected)
      assertEquals(first.virtualSourceName, "<quasiquotes-generated:u001-aux-alias>")
      assertEquals(structure(first.tree), structure(second.tree))
      val trees = allTrees(first.tree)
      assertEquals(trees.size, 18)
      trees.foreach { tree =>
        assert(tree.source.exists)
        assertEquals(tree.source.path, first.virtualSourceName)
        assertEquals(tree.source.content.mkString, expected)
        assert(tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
        directChildren(tree).foreach { child =>
          assert(child.span.start >= tree.span.start)
          assert(child.span.end <= tree.span.end)
        }
      }
      assertEquals(first.tree.span.start, 0)
      assertEquals(first.tree.span.end, expected.length)

      val sourceFree = AuxTypeAliasUntypedLowerer
        .lower(validated)
        .fold(problem => fail(problem.message), identity)
      assert(allTrees(sourceFree).forall(tree => !tree.source.exists && !tree.span.exists))
    }
  }

  test("generated-origin validation rejects invalid virtual source names") {
    withContext {
      val validated = AuxTypeAliasUntypedLoweringInput
        .validate(canonicalDescription())
        .fold(problem => fail(problem.message), identity)
      Vector(null, "", "bad\nname.scala", "bad\rname.scala", "bad\u0000name.scala")
        .foreach { name =>
          assertEquals(
            AuxTypeAliasGeneratedOriginAdapter
              .lower(validated, name)
              .left
              .toOption
              .map(_.code),
            Some("GENERATED_ORIGIN_FAILED")
          )
        }
    }
  }

  test("fully renamed legal input preserves binder roles and exact generated structure") {
    withContext {
      val first = BinderId(11)
      val second = BinderId(12)
      val output = BinderId(13)
      val description = Description(
        aliasName = "Evidence",
        parameters = Vector(
          TypeParameter(first, "Left", None, Some(SourceName("Domain"))),
          TypeParameter(second, "Right", None, Some(SourceName("Domain"))),
          TypeParameter(output, "Result0", None, Some(SourceName("Domain")))
        ),
        target = Applied(
          SourceName("Combine"),
          Vector(
            BinderReference(first, "Left"),
            BinderReference(second, "Right")
          )
        ),
        refinements = Vector(
          DirectTypeAlias(
            "Result",
            Vector.empty,
            None,
            None,
            Vector.empty,
            BinderReference(output, "Result0")
          )
        )
      )
      val validated = AuxTypeAliasUntypedLoweringInput
        .validate(description)
        .fold(problem => fail(problem.message), identity)
      val positioned = AuxTypeAliasGeneratedOriginAdapter
        .lower(validated, "generated/RenamedAuxAlias.scala")
        .fold(problem => fail(problem.message), identity)

      assertEquals(
        positioned.generatedSource,
        "type Evidence[Left <: Domain, Right <: Domain, Result0 <: Domain] = Combine[Left, Right] { type Result = Result0 }"
      )
      assertEquals(
        structure(positioned.tree),
        "TypeDef(Evidence,0,TypeLambda(List(TypeDef(Left,259,TypeBounds(Empty,Ident(Domain),Empty)), TypeDef(Right,259,TypeBounds(Empty,Ident(Domain),Empty)), TypeDef(Result0,259,TypeBounds(Empty,Ident(Domain),Empty))),Refined(Applied(Ident(Combine),List(Ident(Left), Ident(Right))),List(TypeDef(Result,0,Ident(Result0))))))"
      )
    }
  }

  private def canonicalDescription(): Description =
    val first = BinderId(1)
    val second = BinderId(2)
    val output = BinderId(3)
    Description(
      aliasName = "Aux",
      parameters = Vector(
        TypeParameter(first, "N", None, Some(SourceName("Nat"))),
        TypeParameter(second, "M", None, Some(SourceName("Nat"))),
        TypeParameter(output, "Out0", None, Some(SourceName("Nat")))
      ),
      target = Applied(
        SourceName("Add"),
        Vector(BinderReference(first, "N"), BinderReference(second, "M"))
      ),
      refinements = Vector(
        DirectTypeAlias(
          "Out",
          Vector.empty,
          None,
          None,
          Vector.empty,
          BinderReference(output, "Out0")
        )
      )
    )

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.LambdaTypeTree =>
        s"TypeLambda(${value.tparams.map(structure)},${structure(value.body)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.RefinedTypeTree =>
        s"Refined(${structure(value.tpt)},${value.refinements.map(structure)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.LambdaTypeTree => value.tparams.toVector :+ value.body
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty
