package quasiquotes.phase116

import quasiquotes.matching.{BlockPatternStatement, QuasiPattern, TermPattern}

class P2LocalValConstructionTest extends munit.FunSuite:
  test("qr constructs and typechecks one explicitly typed local immutable val") {
    assertEquals(P2LocalValMacros.construct(41), 41)
    assertEquals(P2LocalValMacros.constructList(List(1, 2)), List(1, 2))
  }

  test("qr creates a truthful local symbol owner and keeps the initializer outside its scope") {
    assertEquals(
      P2LocalValMacros.symbolOwnerEvidence(7),
      (true, true, true, true)
    )
  }

  test("same-display-name external splice remains external under the generated binder") {
    val x = 29
    assertEquals(P2LocalValMacros.sameDisplayNameExternal(x), (29, true))
  }

  test("owner-sensitive external splice with definitions is rejected without reownership") {
    val message = P2LocalValMacros.ownedDefinitionSpliceRejection
    assertNotEquals(message, "accepted")
    assert(message.toLowerCase.contains("owner"), message)
    assert(message.toLowerCase.contains("local val"), message)
  }

  test("qq matches alpha-equivalent local binder spellings") {
    assert(
      P2LocalValMacros.alphaMatches {
        val renamed: Int = 1
        renamed
      }
    )
  }

  test("qq captures the exact original initializer subtree") {
    assertEquals(
      P2LocalValMacros.captureInitializerIdentity {
        val renamed: Int = 42
        renamed
      },
      (42, true)
    )
  }

  test("qq does not rewrite a free same-text result into the pattern binder") {
    val x = 31
    assert(
      P2LocalValMacros.boundPatternRejectsFreeSameText {
        val renamed: Int = 1
        x
      }
    )
  }

  test("target inspection rejects every residual local-definition family") {
    assertEquals(
      P2LocalValMacros.targetRejectionEvidence,
      (true, true, true, true, true, true)
    )
  }

  test("current-Dotty qr rejects second P2 binders through every admitted recursive route") {
    val secondBinderDiagnostic = "Only one P2 local val binder is admitted per quasiquote tree"
    val evidence = P2LocalValMacros.constructionScopeBoundaryEvidence

    List(evidence._1, evidence._4, evidence._5, evidence._6).foreach { message =>
      assert(message.contains(secondBinderDiagnostic), message)
    }
  }

  test("current-Dotty qr rejects P2 and Lambda1 same-name source shadowing in both directions") {
    val shadowingDiagnostic = "P2 local val source-binder shadowing is unsupported"
    val evidence = P2LocalValMacros.constructionScopeBoundaryEvidence

    List(evidence._2, evidence._3).foreach { message =>
      assert(message.contains(shadowingDiagnostic), message)
    }
  }

  test("current-Dotty qq pattern compilation rejects nested P2 and P2-Lambda1 source shadowing") {
    val secondBinderDiagnostic = "Only one P2 local val binder is admitted per quasiquote tree"
    val shadowingDiagnostic = "P2 local val source-binder shadowing is unsupported"
    val cases = List(
      "{ val x: Int = 1; { val y: Int = 2; y } }" -> secondBinderDiagnostic,
      "(x: Int) => { val x: Int = 1; x }" -> shadowingDiagnostic,
      "{ val x: Int = 1; (x: Int) => x }" -> shadowingDiagnostic
    )

    cases.foreach { case (source, expected) =>
      val message = quasiquotes.matching.QuasiPattern.term(source).fold(_.message, _ => "accepted")
      assert(message.contains(expected), s"$source: $message")
    }
  }

  test("current-Dotty target inspection rejects nested P2 and P2-Lambda1 source shadowing") {
    val secondBinderDiagnostic = "Only one P2 local val binder is admitted per quasiquote tree"
    val shadowingDiagnostic = "P2 local val source-binder shadowing is unsupported"
    val evidence = P2LocalValMacros.targetScopeBoundaryEvidence

    assert(evidence._1.contains(secondBinderDiagnostic), evidence._1)
    assert(evidence._2.contains(shadowingDiagnostic), evidence._2)
    assert(evidence._3.contains(shadowingDiagnostic), evidence._3)
  }

  test("current-Dotty paths retain one P2 binder combined with a distinct-name Lambda1 binder") {
    val evidence = P2LocalValMacros.distinctNameMixedBinderEvidence
    assertEquals(evidence, (true, true, true, true))

    val lambdaThenP2 = QuasiPattern.term("(outer: Int) => { val x: Int = 1; x }").toOption.get
    val p2ThenLambda = QuasiPattern.term("{ val x: Int = 1; (inner: Int) => inner }").toOption.get

    lambdaThenP2.pattern match
      case TermPattern.Lambda1(lambdaId, _, _, TermPattern.Block(
            List(local: BlockPatternStatement.LocalVal),
            _
          )) => assertNotEquals(lambdaId, local.binderId)
      case other => fail(s"unexpected Lambda1/P2 pattern: $other")

    p2ThenLambda.pattern match
      case TermPattern.Block(
            List(local: BlockPatternStatement.LocalVal),
            TermPattern.Lambda1(lambdaId, _, _, _)
          ) => assertNotEquals(local.binderId, lambdaId)
      case other => fail(s"unexpected P2/Lambda1 pattern: $other")
  }
