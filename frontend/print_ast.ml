(* このファイルは抽象構文木（AST）を出力する機能を提供する *)
(* 抽象構文木（AST: Abstract Syntax Tree）を解析して文字列として出力するユーティリティ *)
(* File print_ast.ml *)
(* モジュールのインポート *)

(* 抽象構文木の型定義や構造が定義されているモジュール。 *)
open Ast
(* フォーマットされた文字列を操作するための標準ライブラリ。 *)
open Printf

(* 空文字列を除外し、セミコロンで文字列を連結する補助関数。 *)
let semi str = if str = "" then str else str ^ "; "

(* AST要素を文字列に変換する関数群 *)

(* 文を処理 *)
let rec  ast_stmt ast = match ast with
        CallProc (s, l) -> sprintf "CallProc(\"%s\",[%s])" s
              (List.fold_left  (fun str x  -> (semi str) ^ (ast_exp x)) "" l)
      | Block (dl, sl) -> sprintf "Block([%s],[%s])"
              (List.fold_left (fun str x -> (semi str) ^ (ast_dec x)) "" dl)
              (List.fold_left (fun str x -> (semi str) ^ (ast_stmt x)) "" sl)
      | Assign (v, e) -> sprintf "Assign(%s,%s)" (ast_var v) (ast_exp e)
      | If (e,s,None) -> sprintf "If(%s,%s,None)" (ast_exp e) (ast_stmt s)
      | If (e,s1,Some s2) -> sprintf "If(%s,%s,Some %s)" (ast_exp e) (ast_stmt s1) (ast_stmt s2)
      | While (e,s) -> sprintf "While(%s,%s)" (ast_exp e) (ast_stmt s)
      | NilStmt -> "NilStmt"
and ast_var ast = match ast with
        Var s -> sprintf "Var \"%s\"" s
      | IndexedVar (v, e) -> sprintf "IndexedVar (%s,%s)" (ast_var v) (ast_exp e)
and ast_dec ast = match ast with
        FuncDec (s, l, t, b) ->
            sprintf "FuncDec(\"%s\",[%s],%s,%s)" s
                (List.fold_left (fun str (t,s) -> (semi str) ^ sprintf "(%s,\"%s\")" (ast_typ t) s) "" l)
                (ast_typ t)
                (ast_stmt b)
      | VarDec (t,s) -> sprintf "VarDec(%s,\"%s\")" (ast_typ t) s
      | TypeDec (s, t) -> sprintf "TypeDec (\"%s\",%s)" s (ast_typ t)
and ast_exp ast = match ast with
        VarExp v -> sprintf "VarExp(%s)" (ast_var v)
      | StrExp s -> sprintf "StrExp(%s)" s
      | IntExp i -> sprintf "IntExp(%d)" i
      | CallFunc (s, l) -> sprintf "CallFunc(\"%s\",[%s])" s
                               (List.fold_left (fun str x -> (semi str) ^ (ast_exp x)) "" l)
and ast_typ ast = match ast with
        NameTyp s -> sprintf "NameTyp \"%s\"" s
      | ArrayTyp (size,t) -> sprintf "ArrayTyp (%d,%s)" size (ast_typ t)
      | IntTyp -> "IntTyp"
      | VoidTyp -> "VoidTyp"

let main () =
  (* The open of a file *)
  let cin = if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
            else stdin in
                        let lexbuf = Lexing.from_channel cin in
                                try
                        (* The start of the entire program *)
                                        print_string (ast_stmt (Parser.prog Lexer.lexer lexbuf));
                                        print_string "\n"
                                with
                                | Parsing.Parse_error ->
                                        let pos = lexbuf.Lexing.lex_curr_p in
                                        Printf.eprintf "Syntax error at line %d, near token '%s'\n"
                                        (pos.Lexing.pos_lnum - 1)
                                        (Lexing.lexeme lexbuf);
                                        exit 1

let _ = try main () with
         Parsing.Parse_error -> print_string "syntax error\n"
