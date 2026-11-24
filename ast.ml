type program =
  | Program of program list
  | AFundef of (bool * program * program list * typ * program)
  | AIdent of string
  | AParam of program option * typ
  | ABlock of program list
  | AReturn of program
  | ADecl of (program * program * typ * program)
  | AIf of (program * program * program)
  | AWhile of (program * program)
  | ABreak
  | AContinue
  | ADefer program
  | AAssign (program * program)
  | APrint string * program list
  | AInt of string
  | AUop of uop * program
  | ABop of bop * program * program
  | AUnreachable

and uop =
  | UNot
  | UNeg
 
and bop =
  | BEq
  | BNeq
  | BLt
  | BLeq
  | BGt
  | BGeq
  | BAdd
  | BSub
  | BMul
  | BAnd
  | BOr