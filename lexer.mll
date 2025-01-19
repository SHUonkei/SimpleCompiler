(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule lexer = parse
| digit+ as num  { NUM (int_of_string num) }
| "if"                    { IF }
| "else"                  { ELSE }
| "do"                    { DO }
| "while"                 { WHILE }
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
| "+="                    { PLUS_ASSIGN }
| "=="                    { EQ }
| "!="                    { NEQ }
| '>'                     { GT }
| '<'                     { LT }
| ">="                    { GE }
| "<="                    { LE }
| "++"                    { INCRIMENT }
| '+'                     { PLUS }
| '-'                     { MINUS }
| '%'                     { MOD }
| '*'                     { TIMES }
| "//" [^ '\n']*          { lexer lexbuf }
| '/'                     { DIV }
| '^'                     { POW }
| '{'                     { LB  }
| '}'                     { RB  }
| '['                     { LS }
| ']'                     { RS }
| '('                     { LP  }
| ')'                     { RP  }
| ','                     { COMMA }
| ';'                     { SEMI }
| ['\n']                  { Lexing.new_line lexbuf; lexer lexbuf }(* eat up whitespace *) 
| [' ' '\t']              { lexer lexbuf }(* eat up whitespace *) 
| eof                     { raise End_of_file }
| _                       { raise No_such_symbol }
