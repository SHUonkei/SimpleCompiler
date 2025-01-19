

(* The definition of the abstract syntax tree *)
type id = string
type var = Var of id | IndexedVar of var * exp
and stmt = Assign of var * exp        
        | CallProc of id * (exp list)
        | Block of (dec list) * (stmt list)
        | If of exp * stmt * (stmt option)
        | While of exp * stmt
        | NilStmt
and exp = VarExp of var | StrExp of string | IntExp of int 
        | CallFunc of id * (exp list)
and dec = FuncDec of id * ((typ*id) list) * typ * stmt
        | TypeDec of id * typ
        | VarDecInit of typ * id * exp       (* int x = 2; のような初期化付き宣言 *)
        | VarDec of typ * id                 (* int x;  のような宣言だけ *)
and typ = NameTyp of string
        | ArrayTyp of int * typ
        | IntTyp
        | VoidTyp

