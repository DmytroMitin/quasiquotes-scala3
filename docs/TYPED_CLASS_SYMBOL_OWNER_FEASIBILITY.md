# Typed class, symbol, and owner feasibility

This page records a test-only public-reflection feasibility baseline for the
N1 generated-subclass and N4 anonymous-implementation north stars. It does not
add class quasiquote syntax, anonymous-class support, a Symbol quasiquote, a
general class builder, or a public API.

The dedicated `frontend` fixture compiles and runs on Scala 3.3.8, 3.8.4, and
3.9.0-RC1. It creates a fresh local class below the active splice owner,
extends a caller-selected reflected parent Type, creates one overriding method
below that class, constructs the `ClassDef` and `DefDef`, invokes the generated
parameterless primary constructor, and calls the override at runtime.

## Public-reflection route

The common three-line route is entirely in `quotes.reflect`:

```text
Symbol.newClass(spliceOwner, ..., parents, declarations, None)
  declarations(classSymbol) =
    Symbol.newMethod(classSymbol, ..., methodType, Flags.Override, noSymbol)

ClassDef(classSymbol, parentTrees, List(DefDef(overrideSymbol, rhsCallback)))
Apply(Select(New(TypeIdent(classSymbol)), classSymbol.primaryConstructor), Nil)
Block(List(classDefinition), overrideInvocation)
```

`DefDef` supplies its generated parameter symbols to the body callback. The
backend therefore does not need a caller-supplied parameter `Symbol`. The
probe verifies that the class owner is the active splice owner, the override
owner is the generated class, the primary-constructor owner is the generated
class, the method has `Flags.Override`, and the method reports the inherited
`map` declaration through `allOverriddenSymbols`.

| Reflection surface | 3.3.8 | 3.8.4 | 3.9.0-RC1 |
| --- | --- | --- | --- |
| basic parameterless `Symbol.newClass` | public, experimental | public | public |
| `ClassDef.apply` | public, experimental | public | public |
| `Symbol.newMethod`, `DefDef`, `New`, `Select`, `Apply`, `Block` | public | public | public |
| `Flags.Override`, `owner`, `primaryConstructor`, `Tree.changeOwner` | public | public | public |
| richer constructor-aware `newClass` overloads | absent | public | public |

Scala 3.3.8 requires `scala.annotation.experimental` at the fixture boundary.
The basic common overload creates a public parameterless constructor. Scala
3.8.4 and 3.9.0-RC1 additionally expose overloads for a single term-parameter
clause and for a general `MethodOrPoly` constructor plan. A backend promising
all three lines cannot make those newer overloads its only constructor route.

No exact `dotty.tools.dotc` operation is required for this baseline. Compiler
implementation details remain an oracle for public API behavior, not a reason
to move the route into `dottyInternal`.

## Captures and ownership

The generated override uses the caller-owned reflected `Term` unchanged. Both
an inline literal and a caller local reference compile and return the expected
runtime result. The test verifies that the exact captured tree object remains
in the constructed override body; it is not normalized through the
compiler-free core, copied, or subjected to `changeOwner`.

That result is deliberately narrow. A reference to a caller local is a lexical
capture, not a definition whose symbol should be moved into the generated
method. By contrast, a generated method symbol created below the splice owner
is detached from the generated class and is rejected before `ClassDef`
assembly. Future externally supplied `DefDef`, `ValDef`, or `ClassDef` trees
still need an explicit rebuild/reownership contract. This probe does not
authorize generic owner repair or arbitrary `Tree.changeOwner` use.

## Anonymous implementation comparison

Quoted source of the form `new Phase138Mapper { ... }` contains a synthetic
`ClassDef`, an override `DefDef`, a superclass-constructor call in the parent
list, and a separate application of the synthetic class's primary constructor.
The public backend plan is therefore substantially the same as the named local
class plan. "Anonymous" is syntax-facing naming and presentation; typed
lowering still needs a class symbol, member symbols, constructor selection,
and a containing block.

The present Definition IR is not a class-body plan. A future class slice needs
a class carrier, an override-method carrier, a dynamic parent Type, a
constructor/new term, and ordered owned body definitions. Runtime-length body
definition splices remain a separate prerequisite for the full N4 example.

## Override and constructor boundaries

For the bounded non-overloaded case, the backend can derive the overriding
method symbol from its class owner, source-like name, parameter/result Types,
method type, and `Flags.Override`. It does not need the caller to interpolate
the parent method's raw symbol. Inspecting the inherited declaration is useful
as validation evidence; ordinary compiler typing remains the semantic oracle
for actual override compatibility. A first implementation must exclude
overloaded parent methods until it has an explicit selection rule.

The parameterless constructor is fully derivable from
`classSymbol.primaryConstructor`. Invocation uses `New(TypeIdent(classSymbol))`,
`Select` by that constructor symbol, and `Apply`. A lexical capture works on
all three lines without turning the capture into a constructor parameter.
Constructor parameters and nontrivial parent constructors are later,
separately admitted shapes; the newer constructor-aware overloads do not erase
the Scala 3.3.8 compatibility constraint.

## Symbol decision and next gate

The construction decision is **S1 — no public Symbol quasiquote family**.
Routine class, method, parameter, and constructor symbols are derivable from
the supported source-like plan and reflected Types/Terms. A raw caller-supplied
`q.reflect.Symbol` would duplicate backend work without enabling the bounded
operation. Symbol matching or extraction may later need a different semantic
design, but this construction result does not decide or authorize it.

The one next typed/public gate is:

> implement a bounded internal `frontend` generated-class plan and public-
> reflection lowerer for one local parameterless class, one caller-supplied
> parent `TypeRepr`, one non-overloaded single-parameter override, and one
> unchanged literal-or-local caller `Term` capture, without adding public
> class/Symbol quasiquote syntax.

That gate should turn the test oracle into a narrow project-owned backend
contract. Anonymous bodies, constructor parameters, parent-constructor
arguments, sequence Definition splices, external definition trees, overloads,
matching, and generic owner repair remain outside it.
