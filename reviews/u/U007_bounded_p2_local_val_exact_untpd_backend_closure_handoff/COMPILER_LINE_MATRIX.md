# Compiler-line matrix

| Proof | 3.3.8 | 3.8.4 | 3.9.0-RC1 |
|---|---:|---:|---:|
| parser P2 oracle | PASS | PASS | PASS |
| source-free P2 | PASS | PASS | PASS |
| EmptyFlags/topology parity | PASS | PASS | PASS |
| sidecar preorder | PASS | PASS | PASS |
| binder scope/identity | PASS | PASS | PASS |
| generated source/positions | PASS | PASS | PASS |
| N006 composition | PASS | PASS | PASS |
| direct P2 rejection | PASS | PASS | PASS |
| P1 regression | PASS | PASS | PASS |
| pre-Typer/TASTy/runtime | PASS | PASS | PASS |
| fail-closed matrix | PASS | PASS | PASS |
| complete `dottyInternal/test` | 458/458 | 458/458 | 458/458 |

The exact parser topology, modifiers, and canonical spans did not differ across lines. No version branch was added. The post-review exact selection passed 39/39 and the strengthened `P2LocalValExactBackendTest` passed 9/9 on all three lines before the final complete matrix.
