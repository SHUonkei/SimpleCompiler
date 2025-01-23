

(* The definition of the abstract syntax tree *)
type id = string
type var = Var of id | IndexedVar of var * exp
and stmt = Assign of var * exp
        | CallProc of id * (exp list)
        | Block of (dec list) * (stmt list)
        | If of exp * stmt * (stmt option)
        | While of exp * stmt
        | DoWhile of exp * stmt
        (* | For of var * exp * exp * stmt *)
        | Incr of var
        | NilStmt
and exp = VarExp of var | StrExp of string | IntExp of int
        | CallFunc of id * (exp list)
and dec = FuncDec of id * ((typ*id) list) * typ * stmt
        | TypeDec of id * typ
        | VarDecValue of typ * id * exp
        | VarDec of typ * id
(* and inc = Inc of var *)
and typ = NameTyp of string
        | ArrayTyp of int * typ
        | IntTyp
        | VoidTyp

