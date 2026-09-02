# Binder scope and identity proof

Focused tests establish:

- declaration and result binding is selected by equal `BinderId`;
- hostile `BoundReference.displayName` text never changes emitted reference spelling;
- a free same-text `Identifier` stays a free semantic node before lowering;
- initializer access to the new local `BinderId` fails as out of scope;
- an outer admitted binder is visible in the initializer and later result;
- the local binder is visible in later P1 prefix/result children;
- the local binder map is restored at Block exit in both raw and generated-origin paths, so a corrupt outside sibling reference fails;
- distinct-name Lambda1/P2 composition lowers in either existing admitted direction;
- a Lambda encountered under an ambient definition-body scope remains rejected in both raw and generated-origin paths;
- same-name P2/Lambda shadowing and nested/second P2 are still rejected by Core admission.

The raw compiler representation spells both free and bound references as `Ident`; the semantic distinction remains in `TermShape`/`BinderId` before raw lowering. No symbol is invented to simulate it.
