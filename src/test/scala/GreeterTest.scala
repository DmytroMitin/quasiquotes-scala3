class GreeterTest extends munit.FunSuite:
  test("greeting returns hello world message") {
    assertEquals(Greeter.greeting, "Hello, world!")
  }
