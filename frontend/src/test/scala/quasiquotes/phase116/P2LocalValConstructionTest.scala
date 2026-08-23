package quasiquotes.phase116

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
