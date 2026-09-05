package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.ScopedType
import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.dotty.BoundedExtensionModulePlan.*
import quasiquotes.parser.BinderId

class BoundedExtensionModuleUntypedLowererTest extends munit.FunSuite:
  private final case class Names(
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      argument: String,
      evidence: String,
      evidenceType: String
  ):
    val source =
      s"""object $module:
         |  extension [$typeParameter]($receiver: $typeParameter)
         |    def $method($argument: $typeParameter)(using $evidence: $evidenceType[$typeParameter]): $typeParameter =
         |      $evidence.$method($receiver, $argument)
         |""".stripMargin

  private val canonical =
    Names("syntax", "A", "receiver", "combine", "argument", "evidence", "Semigroup")
  private val renamed =
    Names("operations", "Element", "left", "merge", "right", "instance", "Choice")

  test("lowers canonical and renamed exact plans to parser-equivalent source-free modules") {
    withContext {
      Vector(canonical, renamed).foreach { names =>
        val raw = BoundedExtensionModuleUntypedLowerer
          .lower(validPlan(names))
          .fold(problem => fail(problem.message), identity)
        assertEquals(structure(raw), structure(parseOne(names.source)))
        val trees = allTrees(raw)
        assertEquals(trees.size, 21)
        trees.foreach { tree =>
          assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
          assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }
  }

  test("rejects missing plans and every corrupt exact role without widening topology") {
    withContext {
      assertEquals(
        BoundedExtensionModuleUntypedLowerer.lower(null).left.toOption.map(_.code),
        Some("PLAN_REQUIRED")
      )

      assertEquals(
        create(canonical.copy(module = null)).left.toOption.map(_.code),
        Some("MISSING_FIELD")
      )
      assertEquals(
        create(canonical.copy(module = "")).left.toOption.map(_.code),
        Some("MODULE_NAME_INVALID")
      )
      assertEquals(
        create(canonical.copy(module = "not a name")).left.toOption.map(_.code),
        Some("MODULE_NAME_INVALID")
      )
      assertEquals(
        create(canonical, receiverTypeBinder = BinderId(99)).left.toOption.map(_.code),
        Some("TYPE_PARAMETER_RECEIVER_INVALID")
      )
      assertEquals(
        create(canonical, argumentTypeBinder = BinderId(99)).left.toOption.map(_.code),
        Some("ORDINARY_ARGUMENT_INVALID")
      )
      assertEquals(
        create(canonical, evidenceTypeArguments = Vector(typeReference(canonical), typeReference(canonical)))
          .left.toOption.map(_.code),
        Some("UNARY_EVIDENCE_TYPE_INVALID")
      )
      assertEquals(
        create(canonical, bodyMethod = "different").left.toOption.map(_.code),
        Some("DELEGATED_BODY_INVALID")
      )
      assertEquals(
        create(canonical, bodyArguments = Vector(BodyTermReference(BinderId(1))))
          .left.toOption.map(_.code),
        Some("UNSUPPORTED_TOPOLOGY")
      )
      assertEquals(
        create(canonical.copy(evidence = "not a name")).left.toOption.map(_.code),
        Some("CONTEXTUAL_PARAMETER_INVALID")
      )

      val valid = validPlan(canonical)
      assertEquals(
        BoundedExtensionModulePlan
          .create(
            valid.moduleDisplayName,
            valid.methodDisplayName,
            null,
            valid.receiverParameter,
            valid.ordinaryArgument,
            valid.contextualParameter,
            valid.resultType,
            valid.body
          )
          .left.toOption.map(_.code),
        Some("MISSING_FIELD")
      )
      assertEquals(
        BoundedExtensionModulePlan
          .create(
            valid.moduleDisplayName,
            valid.methodDisplayName,
            valid.typeParameter,
            valid.receiverParameter.copy(binderId = valid.typeParameter.binderId),
            valid.ordinaryArgument,
            valid.contextualParameter,
            valid.resultType,
            valid.body
          )
          .left.toOption.map(_.code),
        Some("TYPE_PARAMETER_RECEIVER_INVALID")
      )
      assertEquals(
        BoundedExtensionModulePlan
          .create(
            valid.moduleDisplayName,
            valid.methodDisplayName,
            valid.typeParameter,
            valid.receiverParameter,
            valid.ordinaryArgument,
            valid.contextualParameter.copy(parameterType = Applied(
              DirectStableSelected(valid.contextualParameter.binderId, "Member"),
              Vector(valid.resultType)
            )),
            valid.resultType,
            valid.body
          )
          .left.toOption.map(_.code),
        Some("UNARY_EVIDENCE_TYPE_INVALID")
      )
      assertEquals(
        BoundedExtensionModulePlan
          .create(
            valid.moduleDisplayName,
            valid.methodDisplayName,
            valid.typeParameter,
            valid.receiverParameter,
            valid.ordinaryArgument,
            valid.contextualParameter,
            valid.resultType,
            valid.body.copy(receiver = BodyTermReference(valid.receiverParameter.binderId))
          )
          .left.toOption.map(_.code),
        Some("DELEGATED_BODY_INVALID")
      )
      Vector(
        Vector(
          BodyTermReference(valid.ordinaryArgument.binderId),
          BodyTermReference(valid.receiverParameter.binderId)
        ),
        Vector(
          BodyTermReference(valid.receiverParameter.binderId),
          BodyTermReference(valid.ordinaryArgument.binderId),
          BodyTermReference(valid.ordinaryArgument.binderId)
        )
      ).foreach { arguments =>
        assert(
          BoundedExtensionModulePlan
            .create(
              valid.moduleDisplayName,
              valid.methodDisplayName,
              valid.typeParameter,
              valid.receiverParameter,
              valid.ordinaryArgument,
              valid.contextualParameter,
              valid.resultType,
              valid.body.copy(arguments = arguments)
            )
            .isLeft
        )
      }
    }
  }

  test("rejects extra raw parameters missing final context and provenance corruption") {
    withContext {
      given SourceFile = NoSource
      val raw = BoundedExtensionModuleUntypedLowerer
        .lower(validPlan(canonical))
        .fold(problem => fail(problem.message), identity)
      val extension = raw.impl.body.head.asInstanceOf[untpd.ExtMethods]
      val method = extension.methods.head.asInstanceOf[untpd.DefDef]
      val extraReceiver = untpd
        .ValDef(termName("extra"), untpd.Ident(typeName("A")), untpd.EmptyTree)
        .withMods(untpd.Modifiers(dotty.tools.dotc.core.Flags.Param))
      val extraReceiverRaw = rebuild(
        raw,
        untpd.ExtMethods(
          List(
            extension.paramss.head,
            extension.paramss(1).map(_.asInstanceOf[untpd.ValDef]) :+ extraReceiver
          ),
          extension.methods
        )
      )
      assertEquals(
        BoundedExtensionModuleUntypedLowerer
          .validateRawCandidate(extraReceiverRaw)
          .left.toOption.map(_.code),
        Some("UNSUPPORTED_COMPILER_TOPOLOGY")
      )

      val missingContextMethod = untpd
        .DefDef(method.name, method.paramss.head :: Nil, method.tpt, method.rhs)
        .withMods(method.mods)
      val missingContextRaw = rebuild(
        raw,
        untpd.ExtMethods(extension.paramss, missingContextMethod :: Nil)
      )
      assertEquals(
        BoundedExtensionModuleUntypedLowerer
          .validateRawCandidate(missingContextRaw)
          .left.toOption.map(_.code),
        Some("UNSUPPORTED_COMPILER_TOPOLOGY")
      )

      val wrongOrderMethod = untpd
        .DefDef(method.name, method.paramss.reverse, method.tpt, method.rhs)
        .withMods(method.mods)
      val wrongOrderRaw = rebuild(
        raw,
        untpd.ExtMethods(extension.paramss, wrongOrderMethod :: Nil)
      )
      assertEquals(
        BoundedExtensionModuleUntypedLowerer
          .validateRawCandidate(wrongOrderRaw)
          .left.toOption.map(_.code),
        Some("UNSUPPORTED_COMPILER_TOPOLOGY")
      )

      val sourceBearing = raw
        .cloneIn(SourceFile.virtual("<quasiquotes-generated:corrupt-u024>", canonical.source))
        .asInstanceOf[untpd.ModuleDef]
      assertEquals(
        BoundedExtensionModuleUntypedLowerer
          .validateRawCandidate(sourceBearing)
          .left.toOption.map(_.code),
        Some("EXACT_RAW_INVARIANT_FAILED")
      )
    }
  }

  private def validPlan(names: Names): Plan =
    create(names).fold(problem => fail(problem.message), identity)

  private def create(
      names: Names,
      receiverTypeBinder: BinderId = BinderId(0),
      argumentTypeBinder: BinderId = BinderId(0),
      evidenceTypeArguments: Vector[ScopedType] = null,
      bodyMethod: String = null,
      bodyArguments: Vector[BodyTermReference] = null
  ): Either[BoundedExtensionModuleError, Plan] =
    val typeBinder = BinderId(0)
    val receiverBinder = BinderId(1)
    val argumentBinder = BinderId(2)
    val evidenceBinder = BinderId(3)
    val reference = typeReference(names)
    val evidenceArguments =
      Option(evidenceTypeArguments).getOrElse(Vector(reference))
    val selected = Option(bodyMethod).getOrElse(names.method)
    val arguments = Option(bodyArguments).getOrElse(
      Vector(BodyTermReference(receiverBinder), BodyTermReference(argumentBinder))
    )
    BoundedExtensionModulePlan.create(
      names.module,
      names.method,
      TypeParameter(typeBinder, names.typeParameter),
      ReceiverParameter(
        receiverBinder,
        names.receiver,
        TypeParameterReference(receiverTypeBinder, names.typeParameter)
      ),
      OrdinaryArgument(
        argumentBinder,
        names.argument,
        TypeParameterReference(argumentTypeBinder, names.typeParameter)
      ),
      ContextualParameter(
        evidenceBinder,
        names.evidence,
        Applied(SourceName(names.evidenceType), evidenceArguments)
      ),
      reference,
      DelegatedBody(BodyTermReference(evidenceBinder), selected, arguments)
    )

  private def typeReference(names: Names): TypeParameterReference =
    TypeParameterReference(BinderId(0), names.typeParameter)

  private def rebuild(
      raw: untpd.ModuleDef,
      extension: untpd.ExtMethods
  )(using Context, SourceFile): untpd.ModuleDef =
    val template = untpd.Template(
      raw.impl.constr,
      raw.impl.parentsOrDerived,
      raw.impl.derived,
      raw.impl.self,
      extension :: Nil
    )
    untpd.ModuleDef(raw.name, template).withMods(raw.mods)

  private def parseOne(source: String)(using outer: Context): untpd.ModuleDef =
    val reporter = new StoreReporter(null)
    given Context = outer.fresh.setReporter(reporter)
    val parsed =
      new Parsers.Parser(SourceFile.virtual("U024ExpectedExtensionModule.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.ModuleDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.ModuleDef =>
        s"ModuleDef(${value.name},${value.mods.flags},${structure(value.impl)})"
      case value: untpd.Template =>
        s"Template(${structure(value.constr)},${value.parentsOrDerived.map(structure)},${value.derived.map(structure)},${structure(value.self)},${value.body.map(structure)})"
      case value: untpd.ExtMethods =>
        s"ExtMethods(${value.paramss.map(_.map(structure))},${value.methods.map(structure)})"
      case value: untpd.DefDef =>
        s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(structure))},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${value.mods.flags},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Apply =>
        s"Apply(${structure(value.fun)},${value.args.map(structure)})"
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.ModuleDef => Vector(value.impl)
      case value: untpd.Template =>
        Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body
      case value: untpd.ExtMethods =>
        value.paramss.flatten.toVector ++ value.methods.toVector
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
