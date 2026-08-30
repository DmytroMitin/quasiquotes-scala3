package quasiquotes.phase138

import scala.annotation.experimental
import scala.quoted.*

trait Phase138Mapper:
  def map(value: Int): Int

final case class Phase138GeneratedEvidence(
    result: Int,
    classOwnedBySplice: Boolean,
    methodOwnedByClass: Boolean,
    constructorOwnedByClass: Boolean,
    methodHasOverrideFlag: Boolean,
    overridesParentMethod: Boolean,
    callerTermObjectRetained: Boolean,
    classTreeUsesRequestedParent: Boolean,
    invocationUsesPrimaryConstructor: Boolean
)

final case class Phase138AnonymousEvidence(
    result: Int,
    classDefinitionCount: Int,
    overrideDefinitionCount: Int,
    constructorApplicationCount: Int
)

@experimental
object Phase138PublicReflectionProbe:
  inline def generated[P <: Phase138Mapper](inline captured: Int, inline input: Int): Phase138GeneratedEvidence =
    ${ generatedImpl[P]('captured, 'input) }

  inline def quotedAnonymous: Phase138AnonymousEvidence =
    ${ quotedAnonymousImpl }

  inline def rejectDetachedMethodOwner(): Unit =
    ${ rejectDetachedMethodOwnerImpl }

  private def generatedImpl[P <: Phase138Mapper: Type](
      captured: Expr[Int],
      input: Expr[Int]
  )(using Quotes): Expr[Phase138GeneratedEvidence] =
    import quotes.reflect.*

    val spliceOwner = Symbol.spliceOwner
    val parentType = TypeRepr.of[P]
    val objectType = TypeRepr.of[Object]
    val methodType = MethodType(List("value"))(
      _ => List(TypeRepr.of[Int]),
      _ => TypeRepr.of[Int]
    )
    var overrideMethod = Symbol.noSymbol
    val generatedClass = Symbol.newClass(
      spliceOwner,
      Symbol.freshName("Phase138GeneratedMapper"),
      List(objectType, parentType),
      cls =>
        overrideMethod = Symbol.newMethod(
          cls,
          "map",
          methodType,
          Flags.Override,
          Symbol.noSymbol
        )
        List(overrideMethod),
      selfType = None
    )

    val originalCapturedTerm = captured.asTerm
    var callerTermObjectRetained = false
    val overrideDefinition = DefDef(overrideMethod, parameterClauses =>
      val parameter = parameterClauses.head.head.asInstanceOf[Term]
      val body = Select.overloaded(
        originalCapturedTerm,
        "+",
        Nil,
        List(parameter),
        TypeRepr.of[Int]
      )
      val identityCheck = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          if tree.asInstanceOf[AnyRef] eq originalCapturedTerm.asInstanceOf[AnyRef] then
            callerTermObjectRetained = true
          super.traverseTree(tree)(owner)
      identityCheck.traverseTree(body)(overrideMethod)
      Some(body)
    )
    val parentTrees = List(TypeTree.of[Object], TypeTree.of[P])
    val classDefinition = ClassDef(generatedClass, parentTrees, List(overrideDefinition))
    val constructor = generatedClass.primaryConstructor
    val constructed = Apply(Select(New(TypeIdent(generatedClass)), constructor), Nil)
    val asParent = Typed(constructed, TypeTree.of[P])
    val invocation = Apply(Select(asParent, overrideMethod), List(input.asTerm))
    val result = Block(List(classDefinition), invocation).asExprOf[Int]

    val classTreeUsesRequestedParent = classDefinition.parents.exists {
      case parent: TypeTree => parent.tpe =:= parentType
      case _ => false
    }
    val invocationUsesPrimaryConstructor =
      constructor == generatedClass.primaryConstructor &&
        constructed.fun.symbol == constructor

    '{
      Phase138GeneratedEvidence(
        $result,
        ${ Expr(generatedClass.owner == spliceOwner) },
        ${ Expr(overrideMethod.owner == generatedClass) },
        ${ Expr(constructor.owner == generatedClass) },
        ${ Expr(overrideMethod.flags.is(Flags.Override)) },
        ${ Expr(overrideMethod.allOverriddenSymbols.exists(_.name == "map")) },
        ${ Expr(callerTermObjectRetained) },
        ${ Expr(classTreeUsesRequestedParent) },
        ${ Expr(invocationUsesPrimaryConstructor) }
      )
    }

  private def quotedAnonymousImpl(using Quotes): Expr[Phase138AnonymousEvidence] =
    import quotes.reflect.*

    val expression = '{
      val mapper = new Phase138Mapper:
        override def map(value: Int): Int = value + 1
      mapper.map(41)
    }
    var classDefinitionCount = 0
    var overrideDefinitionCount = 0
    var constructorApplicationCount = 0
    var anonymousClass = Symbol.noSymbol
    val traverser = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: ClassDef =>
            classDefinitionCount += 1
            anonymousClass = definition.symbol
          case definition: DefDef if definition.symbol.flags.is(Flags.Override) =>
            overrideDefinitionCount += 1
          case Apply(selection: Select, _)
              if selection.symbol.owner == anonymousClass =>
            selection.qualifier match
              case New(_) if selection.symbol == anonymousClass.primaryConstructor =>
                constructorApplicationCount += 1
              case _ => ()
          case _ => ()
        super.traverseTree(tree)(owner)
    traverser.traverseTree(expression.asTerm)(Symbol.spliceOwner)

    '{
      Phase138AnonymousEvidence(
        $expression,
        ${ Expr(classDefinitionCount) },
        ${ Expr(overrideDefinitionCount) },
        ${ Expr(constructorApplicationCount) }
      )
    }

  private def rejectDetachedMethodOwnerImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val generatedClass = Symbol.newClass(
      Symbol.spliceOwner,
      Symbol.freshName("Phase138WrongOwner"),
      List(TypeRepr.of[Object], TypeRepr.of[Phase138Mapper]),
      _ => Nil,
      selfType = None
    )
    val detachedMethod = Symbol.newMethod(
      Symbol.spliceOwner,
      "map",
      MethodType(List("value"))(
        _ => List(TypeRepr.of[Int]),
        _ => TypeRepr.of[Int]
      ),
      Flags.Override,
      Symbol.noSymbol
    )
    if detachedMethod.owner != generatedClass then
      quotes.reflect.report.errorAndAbort(
        "PHASE138_DETACHED_METHOD_OWNER_REJECTED: generated override must be owned by its generated class"
      )
    '{ () }
