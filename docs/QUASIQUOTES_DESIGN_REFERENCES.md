## External Influences and Design References

This section summarizes key sources and traditions that inform the design of quasiquote systems,
and how this project relates to them.

There is no single canonical reference for “how to design quasiquotes”.
Instead, best practices emerge from multiple ecosystems:

- Lisp / Scheme / Racket (semantics)
- Haskell Template Haskell (architecture)
- Scala 2 quasiquotes (ergonomics and pitfalls)

---

### Lisp / Scheme / Racket (semantic foundation)

Quasiquotation originates in Lisp.

Core constructs:

- quasiquote: `( ... )`
- unquote: ,x
- unquote-splicing: ,@xs

Key idea:

syntax templates with holes

Important properties:

- purely syntactic
- compositional
- supports nesting (quasiquote inside quasiquote)
- supports splicing (inserting lists of elements)

Design lessons:

- quasiquotes operate on *syntax*, not values
- holes are *metavariables*, not variables
- repetition of a metavariable naturally implies equality
- splicing is a first-class concept

Racket documentation provides one of the clearest specifications of these rules.

---

### Haskell (Template Haskell quasiquotes)

Haskell provides quasiquotation via Template Haskell.

A quasiquoter is defined as:

- a parser from string to AST
- with entry points:
    - expression
    - pattern
    - type
    - declaration

Key idea:

quasiquotes are user-defined concrete syntax

Important properties:

- parsing is explicit and user-defined
- AST construction is explicit
- pattern matching is supported separately
- types are integrated into the system

Design lessons:

- quasiquotes = parser + AST builder
- pattern and construction sides are separate but related
- syntax is not magical—it is defined by the quasiquoter
- extensibility is essential

---

### Scala 2 quasiquotes

Scala 2 quasiquotes are the closest direct influence.

Key features:

- string interpolator: q"..."
- construction and pattern matching
- antiquotation: $x
- sequence splicing: ..$xs, ...$xss

Conceptual model:

source-like syntax -> AST  
AST <-> pattern matching

Important properties:

- highly ergonomic
- tightly integrated with Scala syntax
- supports both construction and deconstruction

Known challenges:

- implicit normalization rules
- subtle matching behavior
- dependence on tree shapes
- difficulty maintaining consistency

Design lessons:

- ergonomics matter a lot
- symmetry (construct vs match) is powerful
- normalization is unavoidable
- implicit behavior becomes a source of complexity

---

### Scala 3 metaprogramming (contrast)

Scala 3 replaces quasiquotes with:

- quotes/splices ('{ ... }, ${ ... })
- quotes.reflect API

Key properties:

- strongly typed
- explicit staging
- no string-based syntax templates
- low-level tree manipulation via reflect

Design lessons:

- type safety is prioritized over ergonomics
- reflection API is powerful but verbose
- lack of quasiquotes pushes complexity to users

---

### Compiler architecture influence

Across languages, quasiquote systems tend to follow:

string -> parser -> AST -> transformation -> result

This project adopts this explicitly:

- parser reuse (Scala 3 parser)
- explicit lowering phase
- explicit normalization
- explicit matching

Design lessons:

- reuse the language parser whenever possible
- separate parsing from AST construction
- structure the system like a compiler pipeline

---

### What this project adopts

From the above systems, this project follows:

- parser-driven design (Haskell, compiler tradition)
- syntax templates with holes (Lisp)
- construction + matching symmetry (Scala 2)
- explicit phase separation (compiler architecture)
- holes as metavariables (Lisp, Scala 2)

---

### What this project changes

Compared to prior systems:

- normalization is explicit (not implicit as in Scala 2)
- repeated-hole semantics is introduced explicitly
- parsing and lowering are clearly separated
- equality is defined via normalization

---

### What this project intentionally avoids

The project does not attempt:

- full semantic equivalence
- algebraic normalization
- alpha-equivalence
- binder-aware matching
- full compatibility with Scala 2 quasiquotes

These are considered out of scope for a POC.

---

### Summary

Quasiquote design is not defined by a single paradigm.

It is a synthesis of:

- Lisp: semantic model (templates + holes)
- Haskell: architecture (parser + AST builder)
- Scala 2: ergonomics and usability

This project builds on these ideas while making
normalization and equality explicit and controllable.