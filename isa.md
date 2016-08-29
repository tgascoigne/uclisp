



|---------+-----------------------------------------------+-----------------------------------------------+---------------------------------------------------------------------------------------|
| Opcode  | s1                                            | s2                                            | Notes                                                                                 |
|---------+-----------------------------------------------+-----------------------------------------------+---------------------------------------------------------------------------------------|
| NOP     | s, e, NOP.c, d                                | s, e, c, d                                    |                                                                                       |
| LOAD    | s, e, LOAD.V.c, d                             | V.s, e, c, d                                  | Load constant V onto stack                                                            |
| LOOKUP  | sym.s, e, LOOKUP.c, d                         | V.s, e, c, d                                  | Lookup sym in e and push value V.                                                     |
| LOOKUPC | sym.s, e, LOOKUPC.c, d                        | C.s, e, c, d                                  | Lookup the env cell C (sym . V) for symbol sym                                        |
| CONS    | car.cdr.s, e, CONS.c, d                       | C.s, e, c, d                                  | Construct the cell C (car . cdr)                                                      |
| CAR     | C.s, e, CAR.c, d                              | car.s, e, c, d                                |                                                                                       |
| CDR     | C.s, e, CDR.c, d                              | cdr.s, e, c, d                                |                                                                                       |
| SETCAR  | C.V.s, e, SETCAR.c, d                         | s, e, c, d                                    | Set car of C to V                                                                     |
| SETCDR  | C.V.s, e, SETCDR.c, d                         | s, e, c, d                                    | Set cdr of C to V                                                                     |
| APPLY   | (lambda argspec body).(args).s, e, APPLY.c, d | (), (pairlis argspec args).e, body, (s e c).d | Binds args to argspec and transfers control to body. Pushes the current state to dump |
| RETURN  | s, e, RETURN.c, (s' e' c').d                  | (car s).s', e', c', d                         | Restores s', e', c' from dump, and pushes the value on top of the current stack to s' |
| EVAL    | code.s, e, EVAL.c, d                          | V.s, e, c, d                                  | Evaluates the bytecode on s, and pushes the result back to s                          |
| COMPILE | sexpr.s, e, COMPILE.c, d                      | code.s, e, c, d                               | Compiles the sexpr on s and pushes the bytecode back to s                             |
| SELECT  | cond.s, e, SELECT.c1.c2.c, d                  | s, e, c1 or c2, c.d                           | If cond is nil execute c2, otherwise execute c1.                                      |
| JOIN    | s, e, JOIN.c, c'.d                            | s, e, c', d                                   | Breaks from a SELECT condition, restoring c' from d                                   |
| EQUAL   | A.B.s, e, EQUAL.c, d                          | V.s, e, c, d                                  | Compares A and B by value and pushes nil or t to s                                    |
| ADD     | A.B.s, e. ADD.c, d                            | V.s, e, c, d                                  | Pushes the result of A+B to the stack                                                 |
| SUB     | A.B.s, e. SUB.c, d                            | V.s, e, c, d                                  | Pushes the result of A-B to the stack                                                 |
| MUL     | A.B.s, e. MUL.c, d                            | V.s, e, c, d                                  | Pushes the result of A*B to the stack                                                 |
| DIV     | A.B.s, e. DIV.c, d                            | V.s, e, c, d                                  | Pushes the result of A/B to the stack                                                 |
| MOD     | A.B.s, e. MOD.c, d                            | V.s, e, c, d                                  | Pushes the result of A%B to the stack                                                 |
|---------+-----------------------------------------------+-----------------------------------------------+---------------------------------------------------------------------------------------|
