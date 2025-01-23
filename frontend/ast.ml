(* 抽象構文木（AST）のデータ型を定義
ASTの構造（ノードやそのプロパティ）がここで定義され、解析や出力の中心的な役割を果たしている *)

(* The definition of the abstract syntax tree *)
type id = string
type var = Var of id | IndexedVar of var * exp
and stmt =
        (* 変数に値を代入する文です。 ex. x = 5 は Assign(Var("x"), IntExp(5))*)
          Assign of var * exp
        (* プロシージャ（戻り値を持たない関数）の呼び出しを表します。(print関数みたいな) *)
        (* ex. print("Hello") は CallProc("print", [StrExp("Hello")]) *)
        | CallProc of id * (exp list)
        (* 複数の宣言と文を含むブロックを表します。 *)
        (* ex. Block([VarDec(IntTyp, "x")], [Assign(Var("x"), IntExp(5))]) *)
        | Block of (dec list) * (stmt list)
        (* 条件文（if-then-else）を表します。 *)
        (* ex. if x == 5 then print("x is 5") else print("x is not 5") *)
        | If of exp * stmt * (stmt option)
        (* ループ文（while）を表します。 *)
        (* ex. while x < 5 do print(x) *)
        | While of exp * stmt
        (* 空文を表します。 *)
        | NilStmt
and exp =
        (* 変数、文字列、整数値を表します。 *)
        (* ex. 例: x は VarExp(Var("x"))。 *)
        (* ex. 例: "Hello" は StrExp("Hello")。 *)
        (* ex. 例: 42 は IntExp(42)。 *)
        VarExp of var | StrExp of string | IntExp of int
        (* 関数の呼び出しを表します。 *)
        (* 例: sum(1, 2) は CallFunc("sum", [IntExp(1), IntExp(2)])。 *)
        | CallFunc of id * (exp list)
and dec =
        (* 関数の宣言を表します。 *)
        (* ex. FuncDec("sum", [(IntTyp, "a"); (IntTyp, "b")], IntTyp, stmt) *)
        FuncDec of id * ((typ*id) list) * typ * stmt
        (* 型の宣言 *)
        (* ex. type Point = record {x: int; y: int} *)
        | TypeDec of id * typ
        (* 変数の宣言 *)
        (* ex. int x は VarDec(IntTyp, "x")。 *)
        | VarDec of typ * id
and typ =
        (* 名前付きの型 *)
        (* ex. type MyType は NameTyp("MyType")。 *)
        NameTyp of string
        (* 配列型 *)
        (* ex. int[10] は ArrayTyp(10, IntTyp) *)
        | ArrayTyp of int * typ
        (* 整数型 *)
        | IntTyp
        (* 戻り値を持たない型 *)
        | VoidTyp

