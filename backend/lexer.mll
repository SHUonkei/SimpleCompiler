(* OCamlのLexツールを使用した字句解析器の定義ファイル
ソースコードをトークンに分割するための規則 *)
(* File lexer.mll *)
{
 open Parser
 exception No_such_symbol
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*


rule lexer = parse
| "//"                    { line_comment lexbuf}
| digit+ as num  { NUM (int_of_string num) }
| "if"                    { IF }
| "else"                  { ELSE }
| "do"                    { DO }
| "while"                 { WHILE }
| "for"                   { FOR }
| ".."                    { TO }
| "scan"                  { SCAN }
| "sprint"                { SPRINT }
| "iprint"                { IPRINT }
| "int"                   { INT }
| "return"                { RETURN }
| "type"                  { TYPE }
| "void"                  { VOID }
| id as text              { ID text }
| '\"'[^'\"']*'\"' as str { STR str }
| '='                     { ASSIGN }
| "+="                    { AEQ }
| "=="                    { EQ }
| "!="                    { NEQ }
| '>'                     { GT }
| '<'                     { LT }
| ">="                    { GE }
| "<="                    { LE }
| "++"                    { INCR }
| '+'                     { PLUS }
| '-'                     { MINUS }
| '*'                     { TIMES }
| '/'                     { DIV }
| '^'                     { POW }
| '%'                     { MOD }
| '{'                     { LB  }
| '}'                     { RB  }
| '['                     { LS }
| ']'                     { RS }
| '('                     { LP  }
| ')'                     { RP  }
| ','                     { COMMA }
| ';'                     { SEMI }
| ['\n']                  { Lexing.new_line lexbuf; lexer lexbuf }
| [' ' '\t']         { lexer lexbuf }(* eat up whitespace *)
| eof                     { raise End_of_file }
| _                       { raise No_such_symbol }

and line_comment = parse
| '\n' { lexer lexbuf }  (* 改行が現れたら字句解析に戻る *)
| _    { line_comment lexbuf }  (* コメントを継続して処理 *)
