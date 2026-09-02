# Typed class, symbol, and owner feasibility

This page records the public-reflection oracle and the bounded package-private
implementation for the N1 generated-subclass and N4 anonymous-implementation
north stars. It does not add class quasiquote syntax, anonymous-class support,
a Symbol quasiquote, a general class builder, or a public API.

The dedicated `frontend` fixture compiles and runs on Scala 3.3.8, 3.8.4, and
final 3.9.0. It creates a fresh local class below the active splice owner,
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

| Reflection surface | 3.3.8 | 3.8.4 | 3.9.0 |
| --- | --- | --- | --- |
| basic parameterless `Symbol.newClass` | public, experimental | public | public |
| `ClassDef.apply` | public, experimental | public | public |
| `Symbol.newMethod`, `DefDef`, `New`, `Select`, `Apply`, `Block` | public | public | public |
| `Flags.Override`, `owner`, `primaryConstructor`, `Tree.changeOwner` | public | public | public |
| richer constructor-aware `newClass` overloads | absent | public | public |

Scala 3.3.8 requires `scala.annotation.experimental` at the fixture boundary.
The basic common overload creates a public parameterless constructor. Scala
3.8.4 and 3.9.0 additionally expose overloads for a single term-parameter
clause and for a general `MethodOrPoly` constructor plan. A backend promising
all three lines cannot make those newer overloads its only constructor route.

No exact `dotty.tools.dotc` operation is required for this baseline. Compiler
implementation details remain an oracle for public API behavior, not a reason
to move the route into `dottyInternal`.

## Bounded internal implementation

The `frontend` now contains package-private `GeneratedClassPlan`,
`OverrideMethodPlan`, and `GeneratedMethodBodyPlan` carriers plus a
package-private public-reflection lowerer. The semantic plan records source-like
roles only: active-splice class placement, one caller-provided complete parent
Type, one generated-class-owned override, one generated parameter binder, the
captured-Term-plus-parameter body, and the parameterless primary constructor.
It stores no reflected `Symbol`, no `Any` carrier, no rendered Type, and no raw
compiler tree.

Caller-owned `TypeRepr` and `Term` values remain path-dependent inputs at the
active `Quotes` lowering boundary. The lowerer creates a deterministic fresh
class name from the validated display prefix, creates the method under the
class, obtains the exact parameter `Ref` from the `DefDef` callback, constructs
the class and constructor invocation, and calls the override with the unchanged
caller argument. Tests prove the class/method/constructor owners,
`Flags.Override`, exactly one `allOverriddenSymbols` target, callback binder
identity, parent Type identity, caller capture identity, invocation-argument
identity, and runtime result on all three compiler lines.

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
still need an explicit rebuild/reownership contract. The internal lowerer
rejects a captured Term containing any of those owned definition kinds while
admitting unchanged literal and caller-local references. This result does not
authorize generic owner repair or arbitrary `Tree.changeOwner` use. Foreign or
stale `Quotes` payloads are excluded by the lowerer's path-dependent input
types; it does not cast around that static boundary.

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

## Symbol decision and rotation

The construction decision is **S1 — no public Symbol quasiquote family**.
Routine class, method, parameter, and constructor symbols are derivable from
the supported source-like plan and reflected Types/Terms. A raw caller-supplied
`q.reflect.Symbol` would duplicate backend work without enabling the bounded
operation. Symbol matching or extraction may later need a different semantic
design, but this construction result does not decide or authorize it.

The bounded internal generated-class gate is implemented. N1 and N4 remain
incomplete because there is still no supported class/anonymous syntax, broader
class-body model, or sequence Definition splice. Constructor parameters,
parent-constructor arguments, external definition trees, overloads, matching,
and generic owner repair remain outside the admitted contract.

This completes typed/public rotation slot 2. The next gate rotates to the
neutral/core track. Its default bounded starting point is a compiler-free
`scala.meta.Term` to project-owned `TermShape`/Term-IR projection for the
literal `q"1 + 1"` shape, without inferring broader Term coverage, binder/type
sidecars, a public route, or another typed-only sequence.
