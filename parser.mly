%{

open Printf
open Ast

%}

/* File parser.mly */
%token <int> NUM
%token <string> STR ID
%token INT IF WHILE SPRINT IPRINT SCAN EQ NEQ GT LT GE LE ELSE RETURN NEW DO FOR DOUBLE_DOT
%token PLUS MINUS TIMES DIV LB RB LS RS LP RP ASSIGN PLUS_ASSIGN SEMI COMMA TYPE VOID MOD POW INCRIMENT
%type <Ast.stmt> prog


%nonassoc GT LT EQ NEQ GE LE
%left PLUS MINUS         /* lowest precedence */
%left TIMES DIV MOD INCRIMENT     /* medium precedence */
%nonassoc UMINUS    /* highest precedence */


%start prog           /* the entry point */

%%
// BNF grammar
// prog -> stmt
prog : stmt  {  $1  }
     ;

// ast.mly
// and typ = NameTyp of string
     //    | ArrayTyp of int * typ
     //    | IntTyp
     //    | VoidTyp

ty   : INT { IntTyp } // INTトークンが現れた場合に、ASTノードIntTypを生成。
     | INT LS NUM RS { ArrayTyp ($3, IntTyp) }
     | ID	     { NameTyp $1 }
     ;

decs : decs dec { $1@$2 }
     |          { [] }
     ;
// ast.mly
// and dec = FuncDec of id * ((typ*id) list) * typ * stmt
//         | TypeDec of id * typ
//         | VarDec of typ * id
dec  : ty ID ASSIGN expr SEMI { [VarDecInit ($1, $2, $4)] }
     | ty ids SEMI   { List.map (fun x -> VarDec ($1,x)) $2 }
     | TYPE ID ASSIGN ty SEMI { [TypeDec ($2,$4)] }
     | ty ID LP fargs_opt RP block  { [FuncDec($2, $4, $1, $6)] }
     | VOID ID LP fargs_opt RP block  { [FuncDec($2, $4, VoidTyp, $6)] }
     ; 

ids  : ids COMMA ID    { $1@[$3] }
     | ID              { [$1]  }
     ;

fargs_opt : /* empty */ { [] }
     | fargs            { $1 }
     ;
     
fargs: fargs COMMA ty ID     { $1@[($3,$4)] }
     | ty ID                 { [($1,$2)] }
     ;

stmts: stmts stmt  { $1@[$2] }
     | stmt        { [$1] }
     ;

stmt : ID ASSIGN expr SEMI    { Assign (Var $1, $3) }
     | ID PLUS_ASSIGN expr SEMI { Assign (Var $1, CallFunc ("+", [VarExp (Var $1); $3])) }
     | ID LS expr RS ASSIGN expr SEMI  { Assign (IndexedVar (Var $1, $3), $6) }
     | IF LP cond RP stmt     { If ($3, $5, None) }
     | IF LP cond RP stmt ELSE stmt { If ($3, $5, Some $7) }
     | WHILE LP cond RP stmt  { While ($3, $5) }
     | FOR LP ID ASSIGN expr DOUBLE_DOT expr RP stmt { Block ([VarDec (IntTyp, $3)], [While (CallFunc ("<=", [VarExp (Var $3); $7]), Block ([], [$9; Assign (Var $3, CallFunc ("+", [VarExp (Var $3); IntExp 1]))]))]) }
     | DO block WHILE LP cond RP SEMI { While ($5, $2) }
     | SPRINT LP STR RP SEMI  { CallProc ("sprint", [StrExp $3]) }
     | IPRINT LP expr RP SEMI { CallProc ("iprint", [$3]) }
     | SCAN LP ID RP SEMI  { CallProc ("scan", [VarExp (Var $3)]) }
     | NEW LP ID RP SEMI   { CallProc ("new", [ VarExp (Var $3)]) }
     | ID LP aargs_opt RP SEMI  { CallProc ($1, $3) }
     | RETURN expr SEMI    { CallProc ("return", [$2]) }
     | block { $1 }
     | SEMI { NilStmt }
     ;

aargs_opt: /* empty */     { [] }
        | aargs            { $1 }
        ;

aargs : aargs COMMA expr  { $1@[$3] }
      | expr               { [$1] }
      ;

block: LB decs stmts RB  { Block ($2, $3) }
     ;

expr : NUM { IntExp $1  }
     | ID { VarExp (Var $1) }
     | ID LP aargs_opt RP { CallFunc ($1, $3) } 
     | ID LS expr RS  { VarExp (IndexedVar (Var $1, $3)) }
     | expr INCRIMENT { CallFunc ("++", [$1]) }
     | expr PLUS expr { CallFunc ("+", [$1; $3]) }
     | expr MINUS expr { CallFunc ("-", [$1; $3]) }
     | expr MOD expr { CallFunc ("%", [$1; $3]) }
     | expr TIMES expr { CallFunc ("*", [$1; $3]) }
     | expr DIV expr { CallFunc ("/", [$1; $3]) }
     | expr POW expr { CallFunc ("^", [$1; $3]) }
     | MINUS expr %prec UMINUS { CallFunc("!", [$2]) }
     | LP expr RP  { $2 }
     ;

cond : expr EQ expr  { CallFunc ("==", [$1; $3]) }
     | expr NEQ expr { CallFunc ("!=", [$1; $3]) }
     | expr GT expr  { CallFunc (">", [$1; $3]) }
     | expr LT expr  { CallFunc ("<", [$1; $3]) }
     | expr GE expr  { CallFunc (">=", [$1; $3]) }
     | expr LE expr  { CallFunc ("<=", [$1; $3]) }
     ;
%%
