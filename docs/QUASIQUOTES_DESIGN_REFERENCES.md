## External Influences and Design References

This section summarizes key sources and traditions that inform the design of quasiquote systems,
and how this project relates to them.

There is no single canonical reference for “how to design quasiquotes”.
Instead, best practices emerge from multiple ecosystems:

- Lisp / Scheme / Racket (semantics)
- Haskell Template Haskell (architecture)
- Scala 2 quasiquotes (ergonomics and pitfalls)
- Scala 3 metaprogramming (typed quotes and reflection)
- Squid (typed, hygienic quasiquotes and staged rewriting)
- hearth-cross-quotes (typed quotation compatibility)
- compiler architecture

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

### Squid (typed, hygienic quasiquotes and staged rewriting)

[Squid](https://github.com/epfldata/squid) is a Scala metaprogramming framework focused on type-safe and scope-safe manipulation of program fragments. Its documentation describes type- and scope-safe quasiquotes, reusable intermediate representations, staged rewriting, and optimization-oriented program transformation.

Squid explores a different design point from Scala 2 reflection quasiquotes:

- statically typed code values
- hygienic / scope-safe quasiquotes
- expression-focused program transformation
- staged rewriting and optimization
- reusable intermediate representations

It is useful prior art for:

- typed quasiquote design
- hygiene
- binder-aware matching
- staged program rewriting

This project is different:

- it targets Scala 3
- it is parser-driven over Scala 3 reflection/compiler internals
- it starts with reflect-level quasiquotes (`qr`, `qq`) and TypeRepr-backed type experiments
- it makes normalization and equality explicit as operational matching concerns

Squid does not change the current roadmap. It is relevant future inspiration if this project later explores stronger typing, hygiene, binder-aware matching, staged rewriting, or reusable intermediate representations.

---

### Hearth cross-quotes (typed quotation compatibility)

[hearth-cross-quotes](https://scala-hearth.readthedocs.io/en/stable/cross-quotes/) targets cross-version compatibility around typed quotations and typed macro expressions. Its examples bridge Scala 2-style typed macro expressions such as `c.Expr[A](...)` and Scala 3-style quotes such as `'{ ... }` / `Expr[A]`.

This is closer to Scala 3 standard quotes and Scala 2 typed macro expressions than to Scala 2 untyped reflection quasiquotes such as `q"..."` and `tq"..."`.

This project is different:

- it explores parser-driven reflect-level quasiquotes in Scala 3
- it is closer to Scala 2 `q"..."` / `tq"..."` style tree construction and matching
- compatibility with Scala 2 quasiquotes is not a current priority

hearth-cross-quotes may become relevant later if this project explores:

- a Scala 2 / Scala 3 compatibility layer
- migration between Scala 2 quasiquotes and Scala 3 parser-driven quasiquotes
- typed quote compatibility on top of this project's lower-level quasiquote layer

It does not solve this project's parser-driven quasiquote problem and does not imply that this project should become a compatibility layer.

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
- general alpha-equivalence beyond explicitly modeled bounded binders
- general binder-aware matching beyond the accepted Lambda1 and bounded
  one- and exact-two-parameter definition scopes
- full compatibility with Scala 2 quasiquotes

These are considered out of scope for a POC.

---

### Summary

Quasiquote design is not defined by a single paradigm.

It is a synthesis of:

- Lisp: semantic model (templates + holes)
- Haskell: architecture (parser + AST builder)
- Scala 2: ergonomics and usability
- Squid: typed, hygienic, reusable quasiquotes and staged rewriting
- hearth-cross-quotes: typed quotation compatibility across Scala macro systems

This project builds on these ideas while making
normalization and equality explicit and controllable.
