# Pre-Typer, TASTy, and runtime proof

The runtime fixture inserts generated source:

```scala
{ val x: Int = 41; x + 1 }
```

at the existing pre-Typer seam. Immediately before insertion it proves:

- exact Block with one ValDef and an infix result;
- declaration spelling `x` and initializer topology;
- every raw node is `NoSymbol`;
- every node has the expected virtual source and a contained existing span.

Normal compiler phases then emit both `.class` and `.tasty`. Loading the emitted module and invoking `result` returns `42`, so success depends on the local binding.

This passed on Scala 3.3.8, 3.8.4, and 3.9.0-RC1. It is an internal pre-Typer seam proof, not a public placement or release claim.
